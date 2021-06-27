module Collections

open System

open Types

module Users =
    let internal merge (old: User) (new' : User) =
        let newSourceType, newSourceTime = new'.Sources |> Map.toList |> List.head

        // Check if already registered from earlier source of same type.
        let previousOfSame = old.Sources |> Map.tryFind newSourceType |> Option.defaultValue DateTime.MinValue
        let merged =
            if newSourceTime < previousOfSame then
                old
            else
                let previousOfAny = Util.PreviousSourceOfAny old.Sources

                let sourceIndexOrForum = [ SourceType.Index; SourceType.Forum ]
                let previousIndexOrForum = Util.PreviousSourceOfTypes old.Sources sourceIndexOrForum
                let newIsIndexOrForum = sourceIndexOrForum |> List.contains newSourceType

                let sourceTopicOrProfile = [ SourceType.Topic; SourceType.Profile ]
                let previousTopicOrProfile = Util.PreviousSourceOfTypes old.Sources sourceTopicOrProfile
                let newIsTopicOrProfile = sourceTopicOrProfile |> List.contains newSourceType

                let sourceTopicOrProfileOrMemberlist = [ SourceType.Topic; SourceType.Profile; SourceType.Memberlist ]
                let previousTopicOrProfileOrMemberlist = Util.PreviousSourceOfTypes old.Sources sourceTopicOrProfileOrMemberlist
                let newIsTopicOrProfileOrMemberlist = sourceTopicOrProfileOrMemberlist |> List.contains newSourceType

                {
                    Id         = old.Id
                    Name       =
                        // All source types contains username.
                        if previousOfAny < newSourceTime then
                            new'.Name
                        else
                            old.Name
                    Rank       =
                        // A user can have different ranks in different forums.
                        // Prioritise sources in this order:
                        // 1. Topic
                        // 2. Forum and index when existing field is "User"
                        // 3. Any
                        if newSourceType = SourceType.Topic && previousOfSame < newSourceTime then
                            new'.Rank
                        else if newIsIndexOrForum && previousIndexOrForum < newSourceTime && old.Rank = "User" then
                            new'.Rank
                        else
                            old.Rank
                    CustomRank =
                        // Custom rank is found in topic posts and user profiles.
                        if newIsTopicOrProfile && previousTopicOrProfile < newSourceTime then
                            new'.CustomRank
                        else
                            old.CustomRank
                    JoinDate   =
                        // Join date is found in topic posts, user profiles and member list.
                        if newIsTopicOrProfileOrMemberlist && previousTopicOrProfileOrMemberlist < newSourceTime then
                            new'.JoinDate
                        else
                            old.JoinDate
                    LastActive = old.LastActive // Keep old, do proper merge at end of function.
                    PostCount  =
                        // Post count is found in topic posts, user profiles and member list.
                        if newIsTopicOrProfileOrMemberlist && previousTopicOrProfileOrMemberlist < newSourceTime then
                            new'.PostCount
                        else
                            old.PostCount
                    CanEmail   =
                        // Can email flag is found in topic posts, user profiles and member list.
                        if newIsTopicOrProfileOrMemberlist && previousTopicOrProfileOrMemberlist < newSourceTime then
                            new'.CanEmail
                        else
                            old.CanEmail
                    Avatar     =
                        // Avatar is found in topic posts and user profiles.
                        if newIsTopicOrProfile && previousTopicOrProfile < newSourceTime then
                            new'.Avatar
                        else
                            old.Avatar
                    Location   =
                        // Location is found in topic posts, user profiles and member list.
                        if newIsTopicOrProfileOrMemberlist && previousTopicOrProfileOrMemberlist < newSourceTime then
                            new'.Location
                        else
                            old.Location
                    Homepage   =
                        // Homepage is found in topic posts, user profiles and member list.
                        if newIsTopicOrProfileOrMemberlist && previousTopicOrProfileOrMemberlist < newSourceTime then
                            new'.Homepage
                        else
                            old.Homepage
                    Occupation =
                        // Occupation is only found in user profile.
                        if newSourceType = SourceType.Profile && previousOfSame < newSourceTime then
                            new'.Occupation
                        else
                            old.Occupation
                    Interests  =
                        // Interests field is only found in user profile.
                        if newSourceType = SourceType.Profile && previousOfSame < newSourceTime then
                            new'.Interests
                        else
                            old.Interests
                    XboxTag    =
                        // Xbox Live gamertag is found in topic posts and user profiles.
                        if newIsTopicOrProfile && previousTopicOrProfile < newSourceTime then
                            new'.XboxTag
                        else
                            old.XboxTag
                    AIM        =
                        //AOL Instant Messenger is found in topic posts and user profiles.
                        if newIsTopicOrProfile && previousTopicOrProfile < newSourceTime then
                            new'.AIM
                        else
                            old.AIM
                    YM         =
                        // Yahoo! Messenger is found in topic posts and user profiles.
                        if newIsTopicOrProfile && previousTopicOrProfile < newSourceTime then
                            new'.YM
                        else
                            old.YM
                    MSN        =
                        // MSN Messenger is only found in user profile.
                        if newSourceType = SourceType.Profile && previousOfSame < newSourceTime then
                            new'.MSN
                        else
                            old.MSN
                    ICQ        =
                        // ICQ is found in topic posts and user profiles.
                        if newIsTopicOrProfile && previousTopicOrProfile < newSourceTime then
                            new'.ICQ
                        else
                            old.ICQ
                    Signature  =
                        // Signature is only found in topic posts, but displaying can be
                        // disabled per post, so we will only update if it's set.
                        if newSourceType = SourceType.Topic && previousOfSame < newSourceTime && new'.Signature.IsSome then
                            new'.Signature
                        else
                            old.Signature
                    Sources    = old.Sources.Add(newSourceType, newSourceTime)
                }

        // Always favour most recent activity date, regardless of source timestamp. Old scrapes can have more recent posts than newer scrapes of old topics.
        { merged with LastActive = max old.LastActive new'.LastActive }

    let Set (user : User) (ctx : Context) =
        if ctx.Config.Verbosity > 1 then
            printfn "  Setting user %i '%s'" user.Id user.Name

        let merged =
            match ctx.Users.TryFind user.Id with
            | None -> user
            | Some old  -> merge old user

        {
            ctx with
                Users = ctx.Users.Add (merged.Id, merged)
                Usernames = ctx.Usernames.Add (merged.Name, merged.Id)
        }

    let SetList (users : User list) (ctx : Context) =
        users
        |> List.fold (fun ctx' user -> Set user ctx') ctx

    // Deleted users don't have a visible id. We try to look up by name and assign the first negative id if not found.
    let GuestUserId (name : string) (ctx : Context) =
        let rec findAvailableGuestId id = if ctx.Users.ContainsKey id then findAvailableGuestId (id - 1) else id

        match ctx.Usernames.TryFind name with
        | Some id -> id
        | None -> findAvailableGuestId -1

module Posts =
    let internal merge (old: Post) (new' : Post) =
        let _, newSourceTime = new'.Sources |> Map.toList |> List.head
        let previousOfAny = Util.PreviousSourceOfAny old.Sources

        // Posts can only originate from topic sources, so we're only comparing timestamps.
        if newSourceTime < previousOfAny then
            old
        else
            new'

    let Set (post : Post) (ctx : Context) =
        if ctx.Config.Verbosity > 1 then
            printfn "  Setting post %i in topic %i by user %i" post.Id post.TopicId post.UserId

        let merged =
            match ctx.Posts.TryFind post.Id with
            | None -> post
            | Some old  -> merge old post

        { ctx with Posts = ctx.Posts.Add (merged.Id, merged) }

module Topics =
    let internal merge (old: Topic) (new' : Topic) =
        let newSourceType, newSourceTime = new'.Sources |> Map.toList |> List.head

        // Check if already registered from earlier source of same type.
        let previousOfSame = old.Sources |> Map.tryFind newSourceType |> Option.defaultValue DateTime.MinValue
        if newSourceTime < previousOfSame then
            old
        else
            let previousOfAny = Util.PreviousSourceOfAny old.Sources
            {
                Id           = old.Id
                ForumId      =
                    // All source types contains forum id.
                    if previousOfAny < newSourceTime then
                        new'.ForumId
                    else
                        old.ForumId
                UserId       =
                    // Only forum view contains topic author.
                    if newSourceType = SourceType.Forum && previousOfSame < newSourceTime then
                        new'.UserId
                    else
                        old.UserId
                Title        =
                    // All source types contains topic title.
                    if previousOfAny < newSourceTime then
                        new'.Title
                    else
                        old.Title
                Locked       =
                    // All source types contains locked flag.
                    if previousOfAny < newSourceTime then
                        new'.Locked
                    else
                        old.Locked
                Announcement =
                    // Only forum view has announcement flag.
                    if newSourceType = SourceType.Forum && previousOfSame < newSourceTime then
                        new'.Announcement
                    else
                        old.Announcement
                Sticky       =
                    // Only forum view has sticky flag.
                    if newSourceType = SourceType.Forum && previousOfSame < newSourceTime then
                        new'.Sticky
                    else
                        old.Sticky
                Poll         =
                    // Only topic view has full poll details.
                    if new'.Poll.IsSome && newSourceType = SourceType.Topic && previousOfSame < newSourceTime then
                        new'.Poll
                    else
                        // Forum view has poll flag. Use empty poll if not already set.
                        if new'.Poll.IsSome && old.Poll.IsNone && previousOfAny < newSourceTime then
                            new'.Poll
                        else
                            old.Poll
                Replies      =
                    // Only forum view has reply count.
                    if newSourceType = SourceType.Forum && previousOfSame < newSourceTime then
                        new'.Replies
                    else
                        old.Replies
                Views        =
                    // Only forum view has view count.
                    if newSourceType = SourceType.Forum && previousOfSame < newSourceTime then
                        new'.Views
                    else
                        old.Views
                Sources     = old.Sources.Add(newSourceType, newSourceTime)
            }

    let Set (topic : Topic) (ctx : Context) =
        if ctx.Config.Verbosity > 1 then
            printfn "  Setting topic %i '%s'" topic.Id topic.Title

        let merged =
            match ctx.Topics.TryFind topic.Id with
            | None -> topic
            | Some old  -> merge old topic

        { ctx with Topics = ctx.Topics.Add (merged.Id, merged) }

module Forums =
    let internal merge (old: Forum) (new' : Forum) =
        let newSourceType, newSourceTime = new'.Sources |> Map.toList |> List.head

        // Check if already registered from earlier source of same type.
        let previousOfSame = old.Sources |> Map.tryFind newSourceType |> Option.defaultValue DateTime.MinValue
        if newSourceTime < previousOfSame then
            old
        else
            let previousOfAny = Util.PreviousSourceOfAny old.Sources
            let sourceIndexOrForum = [ SourceType.Index; SourceType.Forum ]
            let previousIndexOrForum = Util.PreviousSourceOfTypes old.Sources sourceIndexOrForum
            let newIsIndexOrForum = sourceIndexOrForum |> List.contains newSourceType
            {
                Id          = old.Id
                Name        =
                    // All source types contains forum name.
                    if previousOfAny < newSourceTime then
                        new'.Name
                    else
                        old.Name

                Description =
                    // Only index contains forum description.
                    if newSourceType = SourceType.Index && previousOfSame < newSourceTime then
                        new'.Description
                    else
                        old.Description
                Moderators  =
                    // Moderators are listed in index and forum views.
                    if newIsIndexOrForum && previousIndexOrForum < newSourceTime then
                        new'.Moderators
                    else
                        old.Moderators
                Order       =
                    // Only index contains forum order.
                    if newSourceType = SourceType.Index && previousOfSame < newSourceTime then
                        new'.Order
                    else
                        old.Order
                TopicCount       =
                    // Only index contains topic count.
                    if newSourceType = SourceType.Index && previousOfSame < newSourceTime then
                        new'.TopicCount
                    else
                        old.TopicCount
                PostCount       =
                    // Only index contains post count.
                    if newSourceType = SourceType.Index && previousOfSame < newSourceTime then
                        new'.PostCount
                    else
                        old.PostCount
                Sources     = old.Sources.Add(newSourceType, newSourceTime)
            }

    let Set (forum : Forum) (ctx : Context) =
        if ctx.Config.Verbosity > 1 then
            printfn "  Setting forum %i '%s'" forum.Id forum.Name

        let merged =
            match ctx.Forums.TryFind forum.Id with
            | None -> forum
            | Some old  -> merge old forum

        { ctx with Forums = ctx.Forums.Add (merged.Id, merged) }
