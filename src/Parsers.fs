module Parsers

open System
open System.Text
open System.Text.RegularExpressions
open FSharp.Data
open FSharp.Data.HtmlActivePatterns

open Types
open Collections

module User =
    let IdFromLink (link : HtmlNode) =
        Util.NumericQueryField link "u"

    let IdAndNameFromProfileLink (link : HtmlNode) =
        IdFromLink link, link.InnerText()

    let idFromPrivMsgLink (doc : HtmlDocument) = IdFromLink(doc.CssSelect("a[href^='privmsg']").Head)
    let nameFromAuthorSearch (doc : HtmlDocument) = Regex.Match(doc.CssSelect("a[href^='search\.php\?search_author']").Head.AttributeValue("href"), @"author=(.+)").Groups.[1].Value
    let customRank (doc : HtmlDocument) = doc.CssSelect("span.postdetails").Head.InnerText().Trim()

    let avatar (doc : HtmlDocument) =
        match doc.CssSelect("table.forumline > tr > td.row1 > img") with
        | img::_ -> Some (img.AttributeValue("src"))
        | [] -> None

    let findRow (doc : HtmlDocument) name =
        (
            doc.CssSelect("table.forumline > tr > td.row1 > table > tr")
            |> List.filter (fun row -> row.InnerText().StartsWith(name + ":"))
        ).Head

    let findField (doc : HtmlDocument) name =
        (findRow doc name).CssSelect("td span.gen").[1].InnerText().Trim()

    let findOptionalField (doc : HtmlDocument) name =
        match findField doc name with
        | "" -> None
        | str -> Some str

    let joined (doc : HtmlDocument) = DateTime.Parse(findField doc "Joined")
    let postCount (doc : HtmlDocument) = Int32.Parse((findField doc "Total posts").Split(' ').[0])
    let CanEmail (node : HtmlNode) =
        match node.CssSelect("a[href^='profile.php?mode=email']") with
        | _::_ -> true
        | [] -> false
    let Homepage (node : HtmlNode) =
        match node.CssSelect("a[target=_userwww]") with
        | a::_ -> Some (a.AttributeValue("href"))
        | [] -> None
    let AIM (node : HtmlNode) =
        match node.CssSelect("a[href^='aim:']") with
        | a::_ -> Some (Regex.Match(a.AttributeValue("href"), @"screenname=(.+)&").Groups.[1].Value.Replace('+', ' '))
        | [] -> None
    let YM (node : HtmlNode) =
        match node.CssSelect("a[href^='http://edit.yahoo.com/']") with
        | a::_ -> Some (Regex.Match(a.AttributeValue("href"), @"target=(.+)&").Groups.[1].Value)
        | [] -> None
    let ICQ (node : HtmlNode) =
        match node.CssSelect("a[href^='http://wwp.icq.com/']") with
        | a::_ -> Some (Regex.Match(a.AttributeValue("href"), @"\?to=(\d+)$").Groups.[1].Value)
        | [] -> None

    let Parse (config : Config) (filename : string) =
        let doc, timestamp = Util.ReadFile config filename

        if not (doc.CssSelect("a[href^='privmsg']").IsEmpty) then
            Users.Set config
                {
                    Id         = idFromPrivMsgLink doc
                    Name       = nameFromAuthorSearch doc
                    Rank       = "User"
                    CustomRank = customRank doc
                    JoinDate   = joined doc
                    PostCount  = postCount doc
                    CanEmail   = CanEmail (findRow doc "E-mail address")
                    Avatar     = avatar doc
                    Location   = findOptionalField doc "Location"
                    Homepage   = findOptionalField doc "Website"
                    Occupation = findOptionalField doc "Occupation"
                    Interests  = findOptionalField doc "Interests"
                    XboxTag    = findOptionalField doc "XboxLiveGamertag"
                    AIM        = AIM (findRow doc "AIM Address")
                    YM         = YM (findRow doc "Yahoo Messenger")
                    MSN        = findOptionalField doc "MSN Messenger"
                    ICQ        = ICQ (findRow doc "ICQ Number")
                    Signature  = None
                    Sources    = Map.empty.Add(SourceType.Profile, timestamp)
                }

module Memberlist =
    let Parse (config : Config) (filename : string) =
        let doc, timestamp = Util.ReadFile config filename

        doc.CssSelect("form > table.forumline > tr").Tail
        |> List.filter(fun row -> not (row.CssSelect("td[class^='row'] a[href^='profile.php']").IsEmpty))
        |> List.iter(fun row ->
            let cols = row.CssSelect("td")
            let user = cols.[1].CssSelect("a[href^='profile.php']").Head

            let id, name = User.IdAndNameFromProfileLink user

            Users.Set config
                {
                    Id         = id
                    Name       = name
                    Rank       = "User"
                    CustomRank = ""
                    JoinDate   = DateTime.Parse(cols.[5].InnerText())
                    PostCount  = Int32.Parse(cols.[2].InnerText())
                    CanEmail   = User.CanEmail cols.[3]
                    Avatar     = None
                    Location   =
                        match cols.[4].InnerText().Trim() with
                        | "" -> None
                        | str -> Some str
                    Homepage   = User.Homepage cols.[7]
                    Occupation = None
                    Interests  = None
                    XboxTag    = None
                    AIM        = None
                    YM         = None
                    MSN        = None
                    ICQ        = None
                    Signature  = None
                    Sources    = Map.empty.Add(SourceType.Memberlist, timestamp)
                }
        )

module Post =
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
            let rec serialize (sb : StringBuilder) html =
                let append (str : string) = sb.Append str |> ignore

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

    let Parse (config : Config) (topicId : int) (userDetails : HtmlNode) (userLinks : HtmlNode) (postTime : HtmlNode) (postBody : HtmlNode) (timestamp : DateTime) =
        let postDetails = userDetails.CssSelect("span[class=postdetails]").Head.InnerText()
        let postBodyContent = postBody.CssSelect("td[colspan=2]").[1]
        let content, signature = Body.Parse(postBodyContent)

        let user =
            let profileLink = userLinks.CssSelect("a[href^='profile.php']")
            let name = userDetails.CssSelect("span[class=name] b").Head.InnerText()
            let customRank = Regex.Match(postDetails, @"^(.*)\n").Groups.[1].Value

            // Existing user
            if not profileLink.IsEmpty then
                {
                    Id   = User.IdFromLink (userLinks.CssSelect("a[href^='profile.php']").Head)
                    Name = name
                    Rank =
                        match userDetails.CssSelect("span[class=postdetails] > img") with
                        | img::_ -> (img.AttributeValue("alt"))
                        | [] -> "User"
                    CustomRank = customRank
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
                    CanEmail = User.CanEmail userLinks
                    Location =
                        let locationMatch = Regex.Match(postDetails, @"Location: (.*)\n")
                        match locationMatch.Success with
                        | true -> Some locationMatch.Groups.[1].Value
                        | false -> None
                    Occupation = None
                    Interests = None
                    Homepage = User.Homepage userLinks
                    XboxTag =
                        let xblDetails = userDetails.CssSelect("div[class=postdetails]").Head.InnerText()
                        let xblMatch = Regex.Match(xblDetails, @"^XboxLiveGamertag:\n(.+)$")
                        match xblMatch.Success with
                        | true -> Some xblMatch.Groups.[1].Value
                        | false -> None
                    AIM = User.AIM userLinks
                    YM = User.YM userLinks
                    MSN = None
                    ICQ = User.ICQ userLinks
                    Signature = signature
                    Sources = Map.empty.Add(SourceType.Topic, timestamp)
                }
            // Guest (deleted)
            else
                User.Stub (Users.GuestUserId name) name customRank timestamp

        Users.Set config user

        Posts.Set config
            {
                Id        =
                    userDetails.CssSelect("a").Head.AttributeValue("name")
                    |> int
                Timestamp = Util.ParseForumTimestamp (postTime.CssSelect("span[class=postdetails]").Head.InnerText().Trim()) timestamp
                UserId    = user.Id
                TopicId   = topicId
                Title     = postBody.CssSelect("td[width='100%'] > span[class=gensmall]").Head.InnerText().Split("Post subject: ").[1]
                Content   = content
                Edited    = ParseEditDetails postBodyContent
                Sources   = Map.empty.Add(SourceType.Topic, timestamp)
            }

module Topic =
    let idFromLink (link : HtmlNode) =
        Util.NumericQueryField link "t"

    let IdAndTitleFromTopicLink (link : HtmlNode) =
        idFromLink link, link.InnerText()

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

    let Parse (config : Config) (filename : string) =
        let doc, timestamp = Util.ReadFile config filename

        let forumLink = doc.CssSelect("head > link[rel=up]")

        if not forumLink.IsEmpty then
            let forum =
                Forum.Stub (Util.NumericQueryField forumLink.Head "f") (forumLink.Head.AttributeValue("title")) timestamp

            Forums.Set config forum

            let topic =
                let id, title = IdAndTitleFromTopicLink(doc.CssSelect("a.cattitlewhite").[2])
                let locked = not (doc.CssSelect("table.forumline > tr > th img[alt^='This topic is locked']").IsEmpty)
                {
                    Id           = id
                    ForumId      = forum.Id
                    UserId       = -1
                    Title        = title
                    Locked       = locked
                    Announcement = false
                    Sticky       = false
                    Poll         = parsePoll doc
                    Replies      = -1
                    Views        = -1
                    Sources      = Map.empty.Add(SourceType.Topic, timestamp)
                }

            Topics.Set config topic

            let userDetails = doc.CssSelect("table.forumline > tr > td[class^='row'][valign='top'][align='left']")
            let postBody    = doc.CssSelect("table.forumline > tr > td[class^='row'][valign='top'][width='100%']")
            let postTime    = doc.CssSelect("table.forumline > tr > td[class^='row'][valign='middle'][align='left']")
            let userLinks   = doc.CssSelect("table.forumline > tr > td[class^='row'][valign='bottom'][width='100%']")

            if config.Verbosity > 2 then
                printfn "    userDetails.Length = %i postBody.Length = %i postBody.Length = %i userLinks.Length = %i" userDetails.Length postBody.Length postTime.Length userLinks.Length

            for i in [0..userDetails.Length-1] do
                Post.Parse config topic.Id userDetails.[i] userLinks.[i] postTime.[i] postBody.[i] timestamp
                |> ignore

module Forum =
    let Parse (config : Config) (filename : string) =
        let doc, timestamp = Util.ReadFile config filename

        // Moderators.
        let moderators =
            doc.CssSelect("form > table[align=center] > tr > td[align=left] > span.gensmallwhite > a[href^='profile.php?mode=viewprofile']")
            |> List.map (fun m ->
                let id, name = User.IdAndNameFromProfileLink(m)
                User.Stub id name "Moderator" timestamp
            )
        moderators |> List.iter (fun m -> Users.Set config m)

        // Current forum.
        let forum =
            let l = doc.CssSelect("form > table[align=center] > tr > td[align=left] > span.cattitlewhite > a.cattitlewhite[href^='viewforum.php']").Head
            let f = Forum.Stub (Util.NumericQueryField l "f") (l.InnerText()) timestamp
            { f with Moderators = moderators |> List.map (fun m -> m.Id) }
        Forums.Set config forum

        // Topic rows in forum table.
        doc.CssSelect("form > table.forumline > tr").Tail
        |> List.filter(fun row -> not (row.CssSelect("td.row1 a[href^='viewtopic.php']").IsEmpty))
        |> List.iter(fun row ->
            let id, title = Topic.IdAndTitleFromTopicLink(row.CssSelect("a.topictitle").Head)
            let locked = not (row.CssSelect("td.row1 > img[alt^='This topic is locked']").IsEmpty)
            let flags = row.CssSelect("span.topictitle > b")
            let hasFlag (flag : string) = not (flags |> List.filter(fun f -> f.InnerText().Contains(flag)) |> List.isEmpty)

            // Topic author.
            let authorLink = row.CssSelect("td.row3 a[href^='profile.php?mode=viewprofile']")
            let authorId, authorName, authorRank =
                // Existing user
                if not authorLink.IsEmpty then
                    let id, name = User.IdAndNameFromProfileLink(authorLink.Head)
                    id, name, "User"
                // Guest (deleted)
                else
                    let name = row.CssSelect("td.row3").Head.InnerText().Trim()
                    let id = Users.GuestUserId name
                    id, name, "Guest"

            Users.Set config (User.Stub authorId authorName authorRank timestamp)

            // Last posting user.
            let lastPosterLink = row.CssSelect("td.row2 a[href^='profile.php?mode=viewprofile']")
            let lastPosterId, lastPosterName, lastPosterRank =
                // Existing user
                if not lastPosterLink.IsEmpty then
                    let id, name = User.IdAndNameFromProfileLink(lastPosterLink.Head)
                    id, name, "User"
                // Guest (deleted)
                else
                    let name = Regex.Match(row.CssSelect("td.row2").[1].InnerText().Trim(), @"\s(\S+)$").Groups.[1].Value
                    let id = Users.GuestUserId name
                    id, name, "Guest"
            Users.Set config (User.Stub lastPosterId lastPosterName lastPosterRank timestamp)

            Topics.Set config
                {
                    Id           = id
                    ForumId      = forum.Id
                    UserId       = authorId
                    Title        = title
                    Locked       = locked
                    Announcement = hasFlag "Announcement"
                    Sticky       = hasFlag "Sticky"
                    Poll         =
                        match (hasFlag "Poll") with
                        | true -> Some { Question = ""; Options = []; Votes = 0 }
                        | false -> None
                    Replies      = Int32.Parse(row.CssSelect("td.row2 > span.viewforumdetails").Head.InnerText())
                    Views        = Int32.Parse(row.CssSelect("td.row3Right > span.viewforumdetails").Head.InnerText())
                    Sources      = Map.empty.Add(SourceType.Forum, timestamp)
                }
        )

module IndexParser =
    let Parse (config : Config) (filename : string) =
        let doc, timestamp = Util.ReadFile config filename

        // Newest registered user.
        let newestId, newestName = User.IdAndNameFromProfileLink(doc.CssSelect("span.gensmallwhite > strong > a[href^='profile.php?mode=viewprofile']").Head)
        Users.Set config (User.Stub newestId newestName "User" timestamp)

        // Find forum rows in index page table.
        doc.CssSelect("table.forumline > tr")
        |> List.filter (fun row -> not (row.CssSelect("td.row1[width='100%']").IsEmpty))
        |> List.iteri(fun i row ->
                // Last posting user.
                let lastPosterId, lastPosterName = User.IdAndNameFromProfileLink(row.CssSelect("td.row2").[2].CssSelect("a[href^='profile.php?mode=viewprofile']").Head)
                Users.Set config (User.Stub lastPosterId lastPosterName "User" timestamp)

                // Moderators.
                let moderators =
                    row.CssSelect("td.row2").[3].CssSelect("a[href^='profile.php?mode=viewprofile']")
                    |> List.map (fun m ->
                        let id, name = User.IdAndNameFromProfileLink(m)
                        User.Stub id name "Moderator" timestamp
                    )
                moderators |> List.iter (fun m -> Users.Set config m)

                let forumLink = row.CssSelect("td.row1 > span.forumlink > a.forumlink").Head
                Forums.Set config
                    {
                        Id          = Util.NumericQueryField forumLink "f"
                        Name        = forumLink.InnerText()
                        Description = row.CssSelect("td.row1 > span.genmed").Head.InnerText().Trim()
                        Moderators  = moderators |> List.map (fun m -> m.Id)
                        Order       = i
                        TopicCount  = Int32.Parse(row.CssSelect("td.row2 > span.gensmall").[0].InnerText())
                        PostCount   = Int32.Parse(row.CssSelect("td.row2 > span.gensmall").[1].InnerText())
                        Sources     = Map.empty.Add(SourceType.Index, timestamp)
                    }
            )
