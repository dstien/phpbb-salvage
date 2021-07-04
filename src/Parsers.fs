module Parsers

open System
open System.Text
open System.Text.RegularExpressions
open FSharp.Data
open FSharp.Data.HtmlActivePatterns

open Types
open Collections

module Profile =
    let LinkSelector = "a[href^='profile.php?mode=viewprofile']"

    let IdFromLink (link : HtmlNode) =
        Util.NumericQueryField link "u"

    let IdAndNameFromProfileLink (link : HtmlNode) =
        IdFromLink link, link.InnerText()

    let UserStubFromProfileLink (link : HtmlNode) (rank : string) (ctx : Context) =
        let id, name = IdAndNameFromProfileLink link
        Users.Set (User.Stub id name rank ctx.Timestamp) ctx

    let UserStubFromProfileLinkSelector (node : HtmlNode) (selector : string) (rank : string) (ctx : Context) =
        UserStubFromProfileLink (node.CssSelect(selector + LinkSelector).Head) rank ctx

    let idFromPrivMsgLink (node : HtmlNode) = IdFromLink(node.CssSelect("a[href^='privmsg']").Head)
    let nameFromHeader (node : HtmlNode) = Regex.Match(node.CssSelect("th.thHead").Head.InnerText(), @"^Viewing profile \| (.+)$").Groups.[1].Value
    let customRank (node : HtmlNode) = node.CssSelect("span.postdetails").Head.InnerText().Trim()

    let avatar (node : HtmlNode) =
        match node.CssSelect("table.forumline > tr > td.row1 > img") with
        | img::_ -> Some (img.AttributeValue("src"))
        | [] -> None

    let findRow (node : HtmlNode) name =
        (
            node.CssSelect("table.forumline > tr > td.row1 > table > tr")
            |> List.filter (fun row -> row.InnerText().StartsWith(name + ":"))
        ).Head

    let findField (node : HtmlNode) name =
        (findRow node name).CssSelect("td span.gen").[1].InnerText().Trim()

    let findOptionalField (node : HtmlNode) name =
        match findField node name with
        | "" -> None
        | str -> Some str

    let postCount (node : HtmlNode) = Int32.Parse((findField node "Total posts").Split(' ').[0])
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

    let Parse (ctx : Context) =
        match (ctx.Html.CssSelect("a[href^='privmsg']").IsEmpty) with
        | true -> ctx
        | false ->
            let joinDate = DateTime.Parse(findField ctx.Html "Joined")

            Users.Set
                {
                    Id         = idFromPrivMsgLink ctx.Html
                    Name       = nameFromHeader ctx.Html
                    Rank       = "User"
                    CustomRank = customRank ctx.Html
                    JoinDate   = joinDate
                    LastActive = joinDate
                    PostCount  = postCount ctx.Html
                    CanEmail   = CanEmail (findRow ctx.Html "E-mail address")
                    Avatar     = avatar ctx.Html
                    Location   = findOptionalField ctx.Html "Location"
                    Homepage   = findOptionalField ctx.Html "Website"
                    Occupation = findOptionalField ctx.Html "Occupation"
                    Interests  = findOptionalField ctx.Html "Interests"
                    XboxTag    = findOptionalField ctx.Html "XboxLiveGamertag"
                    AIM        = AIM (findRow ctx.Html "AIM Address")
                    YM         = YM (findRow ctx.Html "Yahoo Messenger")
                    MSN        = findOptionalField ctx.Html "MSN Messenger"
                    ICQ        = ICQ (findRow ctx.Html "ICQ Number")
                    Signature  = None
                    Sources    = Map.empty.Add(SourceType.Profile, ctx.Timestamp)
                }
                ctx

module Memberlist =
    let internal userRow (row : HtmlNode) (ctx : Context) =
        let cols = row.CssSelect("td")
        let user = cols.[1].CssSelect("a[href^='profile.php']").Head

        let id, name = Profile.IdAndNameFromProfileLink user
        let joinDate = DateTime.Parse(cols.[5].InnerText())

        Users.Set
            {
                Id         = id
                Name       = name
                Rank       = "User"
                CustomRank = ""
                JoinDate   = joinDate
                LastActive = joinDate
                PostCount  = Int32.Parse(cols.[2].InnerText())
                CanEmail   = Profile.CanEmail cols.[3]
                Avatar     = None
                Location   =
                    match cols.[4].InnerText().Trim() with
                    | "" -> None
                    | str -> Some str
                Homepage   = Profile.Homepage cols.[7]
                Occupation = None
                Interests  = None
                XboxTag    = None
                AIM        = None
                YM         = None
                MSN        = None
                ICQ        = None
                Signature  = None
                Sources    = Map.empty.Add(SourceType.Memberlist, ctx.Timestamp)
            }
            ctx

    let Parse (ctx : Context) =
        ctx.Html.CssSelect("form > table.forumline > tr").Tail
        |> List.filter(fun row -> not (row.CssSelect("td[class^='row'] a[href^='profile.php']").IsEmpty))
        |> List.fold (fun ctx' row -> userRow row ctx') ctx

module Post =
    type Nodes = {
        UserDetails : HtmlNode
        UserLinks   : HtmlNode
        PostTime    : HtmlNode
        PostBody    : HtmlNode
    }

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
                // phpbb smilies (create dedicated tag)
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
                            | "exclaim"     -> ":!:"
                            | "question"    -> ":?:"
                            | _ -> sprintf ":%s:" icon
                        HtmlNode.NewElement("E", [| HtmlNode.NewText(smiley) |])
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

        // Generate post text with XML BBcode from translated HTML.
        // Ignore dummy containers. This is the format that phpBB use internally
        // in the database. It is needed because phpBB's reparser tool fails
        // to convert [img] and [quote] tags from plain old BBcode.
        // Based on FSharp.Data's HtmlNode.ToString().
        let bbtext (doc : HtmlNode) =
            let rec serialize (sb : StringBuilder) html =
                let append (str : string) = sb.Append str |> ignore

                match html with
                | HtmlElement(name, attributes, elements) ->
                    // Ignore dummy.
                    if name <> "dummy" then
                        append "<"
                        append (name.Replace("*", "li").ToUpper())
                        if not attributes.IsEmpty then
                            match name with
                            | "quote" ->
                                append " author=\""
                                append (attributes.Head.Value())
                                append "\""
                            | "list" ->
                                append " type=\""
                                append (attributes.Head.Value())
                                append "\""
                            | tag ->
                                append " "
                                append tag
                                append "=\""
                                append (attributes.Head.Value())
                                append "\""
                        append ">"
                        // Smilies don't have BB tags.
                        if name <> "e" then
                            append "<s>["
                            append name
                            if not attributes.IsEmpty then
                                append "="
                                append (attributes.Head.Value())
                            append "]</s>"

                    for element in elements do
                        serialize sb element
                    
                    // Ignore dummy.
                    if name <> "dummy"  && name <> "*" then
                        // Don't close [*] tags and smilies.
                        if name <> "*" && name <> "e" then
                            append "<e>[/"
                            append name
                            append "]</e>"
                        append "</"
                        append (name.ToUpper())
                        append ">"

                | HtmlText str ->
                    append (
                        str
                            .Replace("<", "&lt;") // Escape <
                            .Replace(">", "&gt;") // Escape >
                    )
                | HtmlComment _ -> ()
                | HtmlCData _ -> ()

            let sb = StringBuilder()
            serialize sb doc |> ignore
            sb
                .ToString()
                .Replace("</E><E>", "</E> <E>") // Ensure there's always space between to smileys.

        // Split post and optional signature.
        let splitSignature (body : string) =
            let post = body.Split("\n_________________\n")
            (
                sprintf "<r>%s</r>" post.[0],
                if post.Length > 1 then Some (sprintf "<r>%s</r>" post.[1]) else None)

        // Parse post, return body and signature.
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

    let Parse (topicId : int) (nodes : Nodes) (ctx : Context) =
        let postDetails = nodes.UserDetails.CssSelect("span[class=postdetails]").Head.InnerText()
        let postBodyContent = nodes.PostBody.CssSelect("td[colspan=2]").[1]
        let content, signature = Body.Parse(postBodyContent)
        let timestamp = Util.ParseForumTimestamp (nodes.PostTime.CssSelect("span[class=postdetails]").Head.InnerText().Trim()) ctx.Timestamp

        let user, userType =
            let profileLink = nodes.UserLinks.CssSelect("a[href^='profile.php']")
            let name = nodes.UserDetails.CssSelect("span[class=name] b").Head.InnerText()
            let customRank = Regex.Match(postDetails, @"^(.*)\n").Groups.[1].Value

            match profileLink.IsEmpty with
            // Guest (deleted)
            | true -> None, UserType.Guest name
            // Existing user
            | false ->
                let id = Profile.IdFromLink (nodes.UserLinks.CssSelect(Profile.LinkSelector).Head)
                Some {
                    Id         = id
                    Name       = name
                    Rank       =
                        match nodes.UserDetails.CssSelect("span[class=postdetails] > img") with
                        | img::_ -> (img.AttributeValue("alt"))
                        | [] -> "User"
                    CustomRank = customRank
                    JoinDate   =
                        DateTime.Parse
                            (Regex.Match(postDetails, @"Joined: (\d{1,2} \w{3} \d{4})\n").Groups.[1].Value)
                    LastActive = timestamp
                    Avatar     =
                        match nodes.UserDetails.CssSelect("span[class=postdetails] > div > img") with
                        | img::_ -> Some (img.AttributeValue("src"))
                        | [] -> None
                    PostCount  =
                        Regex.Match(postDetails, @"Posts: (\d+)").Groups.[1].Value
                        |> int
                    CanEmail   = Profile.CanEmail nodes.UserLinks
                    Location   =
                        let locationMatch = Regex.Match(postDetails, @"Location: (.*)\n")

                        match locationMatch.Success with
                        | true -> Some locationMatch.Groups.[1].Value
                        | false -> None
                    Occupation = None
                    Interests  = None
                    Homepage   = Profile.Homepage nodes.UserLinks
                    XboxTag    =
                        let xblDetails = nodes.UserDetails.CssSelect("div[class=postdetails]").Head.InnerText()
                        let xblMatch = Regex.Match(xblDetails, @"^XboxLiveGamertag:\n(.+)$")
                        match xblMatch.Success with
                        | true -> Some xblMatch.Groups.[1].Value
                        | false -> None
                    AIM        = Profile.AIM nodes.UserLinks
                    YM         = Profile.YM nodes.UserLinks
                    MSN        = None
                    ICQ        = Profile.ICQ nodes.UserLinks
                    Signature  = signature
                    Sources    = Map.empty.Add(SourceType.Topic, ctx.Timestamp)
                },
                UserType.Registered id

        let post =
            {
                Id        = nodes.UserDetails.CssSelect("a").Head.AttributeValue("name") |> int
                Timestamp = timestamp
                User      = userType
                TopicId   = topicId
                Title     = nodes.PostBody.CssSelect("td[width='100%'] > span[class=gensmall]").Head.InnerText().Split("Post subject: ").[1]
                Content   = content
                Edited    = ParseEditDetails postBodyContent
                Sources   = Map.empty.Add(SourceType.Topic, ctx.Timestamp)
            }

        ctx
        |> Users.SetOptional user
        |> Posts.Set post

module Topic =
    let idFromLink (link : HtmlNode) =
        Util.NumericQueryField link "t"

    let IdAndTitleFromTopicLink (link : HtmlNode) =
        idFromLink link, link.InnerText()

    // Parse poll question, options and results.
    let parsePoll (node : HtmlNode) =
        let table = node.CssSelect("table.forumline > tr > td[class=row2] > table[align=center]")
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

    let Parse (ctx : Context) =
        let forumLink = ctx.Html.CssSelect("head > link[rel=up]")

        match forumLink.IsEmpty with
        | true -> ctx
        | false ->
            let forum = Forum.Stub (Util.NumericQueryField forumLink.Head "f") (forumLink.Head.AttributeValue("title")) ctx.Timestamp

            let topic =
                let id, title = IdAndTitleFromTopicLink(ctx.Html.CssSelect("a.cattitlewhite").[2])
                let status =
                    match ctx.Html.CssSelect("table.forumline > tr > th img[alt^='This topic is locked']").IsEmpty with
                    | true  -> TopicStatus.Unlocked
                    | false -> TopicStatus.Locked

                {
                    Id           = id
                    ForumId      = forum.Id
                    Status       = status
                    Type         = TopicType.Normal
                    UserFirst    = UserType.Unknown
                    UserLast     = UserType.Unknown
                    PostIds      = []
                    Title        = title
                    Poll         = parsePoll ctx.Html
                    Replies      = -1
                    Views        = -1
                    Sources      = Map.empty.Add(SourceType.Topic, ctx.Timestamp)
                }

            let userDetails = ctx.Html.CssSelect("table.forumline > tr > td[class^='row'][valign='top'][align='left']")
            let postBody    = ctx.Html.CssSelect("table.forumline > tr > td[class^='row'][valign='top'][width='100%']")
            let postTime    = ctx.Html.CssSelect("table.forumline > tr > td[class^='row'][valign='middle'][align='left']")
            let userLinks   = ctx.Html.CssSelect("table.forumline > tr > td[class^='row'][valign='bottom'][width='100%']")

            if ctx.Config.Verbosity > 2 then
                printfn "    userDetails.Length = %i postBody.Length = %i postBody.Length = %i userLinks.Length = %i" userDetails.Length postBody.Length postTime.Length userLinks.Length

            let postNodes = [
                for i in [0..userDetails.Length-1] do
                    yield {
                        Post.Nodes.UserDetails = userDetails.[i]
                        Post.Nodes.UserLinks   = userLinks.[i]
                        Post.Nodes.PostTime    = postTime.[i]
                        Post.Nodes.PostBody    = postBody.[i]
                    }
            ]

            ctx
            |> Forums.Set forum
            |> Topics.Set topic
            |> (fun ctx ->
                (
                    postNodes
                    |> List.fold(fun ctx' nodes -> Post.Parse topic.Id nodes ctx') ctx)
                )

module Forum =
    // Topic rows in forum table.
    let internal topicRow (row : HtmlNode) (forum : Forum) (ctx : Context) =
        let id, title = Topic.IdAndTitleFromTopicLink(row.CssSelect("a.topictitle").Head)
        let status =
            match row.CssSelect("td.row1 > img[alt^='This topic is locked']").IsEmpty with
            | true  -> TopicStatus.Unlocked
            | false -> TopicStatus.Locked

        let flags = row.CssSelect("span.topictitle > b")
        let hasFlag (flag : string) = flags |> List.exists(fun f -> f.InnerText().Contains(flag))

        let type' =
            if hasFlag "Announcement" then
                TopicType.Announcement
            else if hasFlag "Sticky" then
                TopicType.Sticky
            else
                TopicType.Normal

        // Topic author.
        let userFirstLink = row.CssSelect("td.row3 a[href^='profile.php?mode=viewprofile']")
        let userFirst, userFirstType =
            // Existing user
            if not userFirstLink.IsEmpty then
                let id, name = Profile.IdAndNameFromProfileLink(userFirstLink.Head)
                Some (User.Stub id name "User" ctx.Timestamp), UserType.Registered id
            // Guest (deleted)
            else
                let name = row.CssSelect("td.row3").Head.InnerText().Trim()
                None, UserType.Guest name

        // Last posting user.
        let userLastLink = row.CssSelect("td.row2 a[href^='profile.php?mode=viewprofile']")
        let userLast, userLastType =
            // Existing user
            if not userLastLink.IsEmpty then
                let id, name = Profile.IdAndNameFromProfileLink(userLastLink.Head)
                Some (User.Stub id name "User" ctx.Timestamp), UserType.Registered id
            // Guest (deleted)
            else
                let name = Regex.Match(row.CssSelect("td.row2").[1].InnerText().Trim(), @"\s(\S+)$").Groups.[1].Value
                None, UserType.Guest name

        ctx
        |> Users.SetOptional userFirst
        |> Users.SetOptional userLast
        |> Topics.Set
            {
                Id           = id
                ForumId      = forum.Id
                Status       = status
                Type         = type'
                UserFirst    = userFirstType
                UserLast     = userLastType
                PostIds      = []
                Title        = title
                Poll         =
                    match (hasFlag "Poll") with
                    | true -> Some { Question = ""; Options = []; Votes = 0 }
                    | false -> None
                Replies      = Int32.Parse(row.CssSelect("td.row2 > span.viewforumdetails").Head.InnerText())
                Views        = Int32.Parse(row.CssSelect("td.row3Right > span.viewforumdetails").Head.InnerText())
                Sources      = Map.empty.Add(SourceType.Forum, ctx.Timestamp)
            }

    // Find and parse topics.
    let internal topicList (forum : Forum) (ctx : Context) =
        ctx.Html.CssSelect("form > table.forumline > tr").Tail
        |> List.filter(fun row -> not (row.CssSelect("td.row1 a[href^='viewtopic.php']").IsEmpty))
        |> List.fold(fun ctx' row -> topicRow row forum ctx') ctx

    let Parse (ctx : Context) =
        // Moderators.
        let moderators =
            ctx.Html.CssSelect("form > table[align=center] > tr > td[align=left] > span.gensmallwhite > a[href^='profile.php?mode=viewprofile']")
            |> List.map (fun m ->
                let id, name = Profile.IdAndNameFromProfileLink(m)
                User.Stub id name "Moderator" ctx.Timestamp
            )

        // Current forum.
        let forum =
            let l = ctx.Html.CssSelect("form > table[align=center] > tr > td[align=left] > span.cattitlewhite > a.cattitlewhite[href^='viewforum.php']").Head
            let f = Forum.Stub (Util.NumericQueryField l "f") (l.InnerText()) ctx.Timestamp
            { f with Moderators = moderators |> List.map (fun m -> m.Id) }

        ctx
        |> Users.SetList moderators
        |> Forums.Set forum
        |> topicList forum

module Index =
    // Newest registered user.
    let internal newestUser (ctx : Context) =
        Profile.UserStubFromProfileLinkSelector ctx.Html "span.gensmallwhite > strong > " "User" ctx

    // Last posting user.
    let internal lastUser (node : HtmlNode) (ctx : Context) =
        Profile.UserStubFromProfileLinkSelector (node.CssSelect("td.row2").[2]) "" "User" ctx

    // Forum record.
    let internal forum (row : HtmlNode) (index : int) (moderators : User list) (ctx : Context) =
        let forumLink = row.CssSelect("td.row1 > span.forumlink > a.forumlink").Head

        Forums.Set
            {
                Id          = Util.NumericQueryField forumLink "f"
                Name        = forumLink.InnerText()
                Description = row.CssSelect("td.row1 > span.genmed").Head.InnerText().Trim()
                Moderators  = moderators |> List.map (fun m -> m.Id)
                Order       = index
                TopicCount  = Int32.Parse(row.CssSelect("td.row2 > span.gensmall").[0].InnerText())
                PostCount   = Int32.Parse(row.CssSelect("td.row2 > span.gensmall").[1].InnerText())
                Sources     = Map.empty.Add(SourceType.Index, ctx.Timestamp)
            }
            ctx

    // Row in forum list.
    let internal forumRow (row : HtmlNode) (index : int) (ctx : Context) =
        // Moderators to be added as users and also linked to forums.
        let moderators =
            row.CssSelect("td.row2").[3].CssSelect(Profile.LinkSelector)
            |> List.map (fun m ->
                let id, name = Profile.IdAndNameFromProfileLink(m)
                User.Stub id name "Moderator" ctx.Timestamp
            )

        lastUser row ctx
        |> Users.SetList moderators
        |> forum row index moderators

    // Find and parse rows in forum list table with index for ordering.
    let internal forumList (ctx : Context) =
        ctx.Html.CssSelect("table.forumline > tr")
        |> List.filter (fun row -> not (row.CssSelect("td.row1[width='100%']").IsEmpty))
        |> List.fold (fun (ctx', i) row -> (forumRow row i ctx'), i + 1) (ctx, 0)
        |> fst

    let Parse (ctx : Context) =
        ctx
        |> newestUser
        |> forumList
