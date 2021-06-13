#!/usr/bin/env -S dotnet fsi
// phpbb-salvage
// Partially recreate phpbb database from public HTML pages.

#r "nuget: FSharp.Data"

open System
open System.Text
open System.Text.RegularExpressions
open FSharp.Data
open FSharp.Data.HtmlActivePatterns

type User = {
    Id         : int
    Name       : string
    Rank       : string
    CustomRank : string
    JoinDate   : DateTime
    PostCount  : int
    Avatar     : string option
    Location   : string option
    Homepage   : string option
    //Email      : string option
    //Occupation : string option
    //Interests  : string option
    XboxTag    : string option
    AIM        : string option
    YM         : string option
    MSN        : string option
    ICQ        : int option
    Signature  : string option
    //ScrapeDate : DateTime
}

type Edited = {
    User  : string
    Count : int
    Last  : DateTime
}

type Post = {
    Id        : int
    Timestamp : DateTime
    User      : User
    Topic     : string
    Content   : string
    Edited    : Edited option
}

type Forum = {
    Id   : int
    Name : string
    //Description : string
}

type PollOption = {
    Text : string
    Votes : int
}

type Poll = {
    Question : string
    Options  : PollOption list
    Votes    : int
}

type Topic = {
    Forum : Forum
    Id    : int
    Title : string
    Poll  : Poll option
}

module Util =
    let ReadFile (filename: string) =
        // Read entire file and remove all newlines. phpbb have inserted <br/> for every newline in post bodies which FSharp.Data substitutes back to newline.
        let src = IO.File.ReadAllText(filename).Replace("\n", "")
        let doc = HtmlDocument.Load(new IO.StringReader(src))
        printfn "--------------------"
        printfn "Parsed %s\n" filename
        // TODO: Get timestamp from file
        doc

module PostParser =
    module Body =
        // Translate post body HTML to use BBCode while accounting for soup of nested and unclosed tags.
        // 1. Replace HTML tags with their BBCode equivalent in the document tree.
        // 2. Recursively translate children.
        // 3. Unknown tags, usually paragraph containers, are renamed to <dummy>
        let rec translateNodes (nodes : HtmlNode list) =
            nodes
            |> List.map (fun node ->
                match node with
                // Strip edit details.
                | HtmlElement ("span", [ HtmlAttribute("class", "gensmall")], _) -> HtmlNode.NewText("")
                // [img]href[/img]
                // phpbb smilies
                | HtmlElement ("img", _, _) ->
                    let src = node.AttributeValue("src")
                    if src.StartsWith("images/smiles/") then
                        let icon = Regex.Match(src, @"icon_([a-z0-9]+)\.").Groups.[1].Value
                        let smiley =
                            match icon with
                            | "biggrin"     -> ":D"
                            | "smile"       -> ":)"
                            | "wink"        -> ";)"
                            | "sad"         -> ":("
                            | "surprised"   -> ":o"
                            | "confused"    -> ":-?"
                            | "cool"        -> "8-)"
                            | "razz"        -> ":P"
                            | "neutral"     -> ":|"
                            | "exclamation" -> ":!:"
                            | "question"    -> ":?:"
                            | _ -> sprintf " :%s: " icon
                        HtmlNode.NewText(smiley)
                    else
                        HtmlNode.NewElement("img", [| HtmlNode.NewText(src) |])
                // [url]href[/url]
                // [url=href]elements[/url]
                | HtmlElement ("a", _, children) ->
                    let href = node.AttributeValue("href")
                    let hrefText = HtmlNode.NewText(href)
                    // Simple url
                    if children.Length = 1 && children.Head = hrefText then
                        HtmlNode.NewElement("url", [| hrefText |])
                    // Url with children elements
                    else
                        HtmlNode.NewElement("url", [| "url", href |], (translateNodes children))
                // [center]elements[/center]
                | HtmlElement ("div", [ HtmlAttribute("align", "center") ], children) -> HtmlNode.NewElement("center", [||], (translateNodes children))
                // [b]elements[/b]
                | HtmlElement ("span", [ HtmlAttribute("style", "font-weight: bold") ], children) -> HtmlNode.NewElement("b", [||], (translateNodes children))
                // [i]elements[/i]
                | HtmlElement ("span", [ HtmlAttribute("style", "font-style: italic") ], children) -> HtmlNode.NewElement("i", [||], (translateNodes children))
                // [u]elements[/u]
                | HtmlElement ("span", [ HtmlAttribute("style", "text-decoration: underline") ], children) -> HtmlNode.NewElement("u", [||], (translateNodes children))
                // [size=size]elements[/size]
                // [color=color]elements[/color]
                | HtmlElement ("span", attr, children) ->
                    if attr.Length = 1 && attr.Head.Name() = "style" then
                        let style = attr.Head.Value().Split(';').[0].Split(':')
                        match style.[0].Trim() with
                        | "color" -> HtmlNode.NewElement("color", [| "color", style.[1].Trim() |], (translateNodes children))
                        | "font-size" ->
                            // Font size was changed from free px size to pct between 1-200.
                            let normalpx = 12
                            let sizepx = Int32.Parse(Regex.Match(style.[1], @"\d+").Value)
                            let sizepct = min 200 (((max 2 sizepx) * 100) / normalpx)
                            HtmlNode.NewElement("size", [| "size", sizepct.ToString() |], (translateNodes children))
                        | _ -> HtmlNode.NewElement("dummy", [||], (translateNodes children))
                    else
                        HtmlNode.NewElement("dummy", [||], (translateNodes children))
                // [list=type]items[/list]
                | HtmlElement ("ol", attr, children) ->
                    let typ =
                        if attr.IsEmpty || attr.Head.Name() <> "type" then
                            "1"
                        else
                            attr.Head.Value()

                    HtmlNode.NewElement("list", [| "list", typ |], (translateNodes children))
                // [list]items[/list]
                | HtmlElement ("ul", _, children) -> HtmlNode.NewElement("list", [||], (translateNodes children))
                // [li]elements[/li]
                | HtmlElement ("li", _, children) -> HtmlNode.NewElement("*", [||], (translateNodes children))
                // [quote]elements[/quote]
                // [quote=username]elements[/quote]
                // [code]elements[/code]
                | HtmlElement ("table", attr, children) ->
                    let container = children.CssSelect("tr > td[class]").Head
                    match container.AttributeValue("class") with
                    | "quote" ->
                        let header = children.CssSelect("tr > td > span[class=genmed] > b").Head
                        if header.InnerText().EndsWith(" wrote:") then
                            let username = header.InnerText().Split(" wrote:").[0]
                            HtmlNode.NewElement("quote", [| "quote", username|], (translateNodes (container.Elements())))
                        else
                            HtmlNode.NewElement("quote", [||], (translateNodes (container.Elements())))
                    | "code" -> HtmlNode.NewElement("code", [||], (translateNodes (container.Elements())))
                    | _ -> HtmlNode.NewElement("dummy", [||], (translateNodes children))
                // Rename unknown tags to "dummy", parse children.
                | HtmlElement (_, _, children) -> HtmlNode.NewElement("dummy", [||], (translateNodes children))
                // Keep text.
                | HtmlText (_) -> node
                // Ignore comments and CData.
                | HtmlComment (_) -> HtmlNode.NewText("")
                | HtmlCData (_) -> HtmlNode.NewText("")
            )

        // Generate post text with BBCode from translated HTML. Ignore dummy containers.
        // Based on FSharp.Data's HtmlNode.ToString().
        let bbtext (doc : HtmlNode) =
            let rec serialize (sb:StringBuilder) html =
                let append (str:string) = sb.Append str |> ignore

                match html with
                | HtmlElement(name, attributes, elements) ->
                    // Ignore dummy.
                    if name <> "dummy" then
                        append "["
                        append name
                        if not attributes.IsEmpty then
                            append "="
                            // TODO: Check what happens if an attribute contains special characters.
                            append (attributes.Head.Value())

                        append "]"

                    for element in elements do
                        serialize sb element
                    
                    // Ignore dummy and don't close [*] tags.
                    if name <> "dummy"  && name <> "*" then
                        append "[/"
                        append name
                        append "]"

                | HtmlText str -> append str
                | HtmlComment _ -> ()
                | HtmlCData _ -> ()

            let sb = StringBuilder()
            serialize sb doc |> ignore
            sb.ToString()

        // Split post and optional signature.
        let splitSignature (body : string) =
            let post = body.Split("\n_________________\n")
            (post.[0], if post.Length > 1 then Some post.[1] else None)

        let Parse (doc : HtmlNode) =
            let translated = translateNodes([ doc ]).Head
            let fulltext = bbtext(translated)
            splitSignature fulltext

    // Parse details about post edits from the end of the post body.
    let ParseEditDetails (doc : HtmlNode) =
        let str = doc.CssSelect("span[class=gensmall]").Head.InnerText().Trim()
        let matches = Regex.Match(str, @"Last edited by (\w+) on (\d+ \w{3} \d{4} \d{2}:\d{2} \w{2}); edited (\d+) time")
        if matches.Success then
            Some {
                User  = matches.Groups.[1].Value
                Count = Int32.Parse(matches.Groups.[3].Value)
                Last  = DateTime.Parse(matches.Groups.[2].Value)
            }
        else
            None

    let Parse (userDetails : HtmlNode) (userLinks : HtmlNode) (postTime : HtmlNode) (postBody : HtmlNode) =
        let postDetails = userDetails.CssSelect("span[class=postdetails]").Head.InnerText()
        let postBodyContent = postBody.CssSelect("td[colspan=2]").[1]
        let content, signature = Body.Parse(postBodyContent)

        let user = {
            Id = userLinks.CssSelect("a[href^='profile.php']").Head.AttributeValue("href")
                .Split("u=").[1]
                .Split("&").[0]
                |> int
            Name = userDetails.CssSelect("span[class=name] b").Head.InnerText()
            Rank =
                match userDetails.CssSelect("span[class=postdetails] > img") with
                | img::_ -> (img.AttributeValue("alt"))
                | [] -> "User"
            CustomRank = Regex.Match(postDetails, @"^(.*)\n").Groups.[1].Value
            JoinDate =
                DateTime.Parse
                    (Regex.Match(postDetails, @"Joined: (\d{1,2} \w{3} \d{4})\n").Groups.[1].Value)
            Avatar =
                match userDetails.CssSelect("span[class=postdetails] > div > img") with
                | img::_ -> Some (img.AttributeValue("src"))
                | [] -> None
            PostCount =
                Regex.Match(postDetails, @"Posts: (\d+)").Groups.[1].Value
                |> int
            Location =
                let locationMatch = Regex.Match(postDetails, @"Location: (.*)\n")
                match locationMatch.Success with
                | true -> Some locationMatch.Groups.[1].Value
                | false -> None
            Homepage =
                match userLinks.CssSelect("a[target=_userwww]") with
                | a::_ -> Some (a.AttributeValue("href"))
                | [] -> None
            XboxTag =
                let xblDetails = userDetails.CssSelect("div[class=postdetails]").Head.InnerText()
                let xblMatch = Regex.Match(xblDetails, @"^XboxLiveGamertag:\n(.+)$")
                match xblMatch.Success with
                | true -> Some xblMatch.Groups.[1].Value
                | false -> None
            AIM =
                match userLinks.CssSelect("a[href^='aim:']") with
                | a::_ -> Some (Regex.Match(a.AttributeValue("href"), @"screenname=(.+)&").Groups.[1].Value.Replace('+', ' '))
                | [] -> None
            YM =
                match userLinks.CssSelect("a[href^='http://edit.yahoo.com/']") with
                | a::_ -> Some (Regex.Match(a.AttributeValue("href"), @"target=(.+)&").Groups.[1].Value)
                | [] -> None
            MSN = None
            ICQ =
                match userLinks.CssSelect("a[href^='http://wwp.icq.com/']") with
                | a::_ -> Some (Int32.Parse(Regex.Match(a.AttributeValue("href"), @"\?to=(\d+)$").Groups.[1].Value))
                | [] -> None
            Signature = signature
        }

        let post = {
            Id =
                userDetails.CssSelect("a").Head.AttributeValue("name")
                |> int
            Timestamp =
                DateTime.Parse
                    (postTime.CssSelect("span[class=postdetails]").Head.InnerText())
            User = user
            Topic = postBody.CssSelect("td[width='100%'] > span[class=gensmall]").Head.InnerText().Split("Post subject: ").[1]
            Content = content
            Edited = ParseEditDetails postBodyContent
        }

        post

module TopicParser =
    // Parse poll question, options and results.
    let parsePoll (doc : HtmlDocument) =
        let table = doc.CssSelect("table.forumline > tr > td[class=row2] > table[align=center]")
        if not table.IsEmpty then
            let header = table.Head.CssSelect("tr > td[colspan=4] > span[class=gen] > b")
            let options = table.Head.CssSelect("tr > td[align=center] > table > tr")
            Some {
                Question = header.Head.InnerText()
                Options =
                    options
                    |> List.map (fun row ->
                            {
                                Text = row.CssSelect("td > span[class=gen]").Head.InnerText()
                                Votes = Int32.Parse(Regex.Match(row.CssSelect("td[align=center] > span[class=gen]").Head.InnerText(), @"^\[ (\d+) \]$").Groups.[1].Value)
                            }
                        )
                Votes = Int32.Parse(Regex.Match(header.[1].InnerText(), @"^Total Votes : (\d+)$").Groups.[1].Value)
            }
        else
            None

    let Parse (filename : string) =
        let doc = Util.ReadFile filename

        let forum =
            let l = doc.CssSelect("head > link[rel=up]").Head
            {
                Id = l.AttributeValue("href").Split("viewforum.php?f=").[1] |> int
                Name = l.AttributeValue("title")
            }

        let topic =
            let t = doc.CssSelect("a.cattitlewhite").[2]
            {
                Forum = forum
                Id = t.AttributeValue("href")
                    .Split("viewtopic.php?t=").[1]
                    .Split("&").[0]
                    |> int
                Title = t.InnerText()
                Poll = parsePoll doc
            }

        printf "Topic = %A\n" topic

        let userDetails = doc.CssSelect("table.forumline > tr > td[class^='row'][valign='top'][align='left']")
        let postBody    = doc.CssSelect("table.forumline > tr > td[class^='row'][valign='top'][width='100%']")
        let postTime    = doc.CssSelect("table.forumline > tr > td[class^='row'][valign='middle'][align='left']")
        let userLinks   = doc.CssSelect("table.forumline > tr > td[class^='row'][valign='bottom'][width='100%']")
        printfn "Topic %d, %d posts" topic.Id userLinks.Length

        for i in [0..userDetails.Length-1] do
            let post = PostParser.Parse userDetails.[i] userLinks.[i] postTime.[i] (postBody.[i])

            if i = 0 then
                printfn "%A" post

        topic
