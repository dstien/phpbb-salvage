module Collections

open System
open System.Linq
open System.Collections.Generic

open Types

module Users =
    let internal dict = new SortedDictionary<int, User>()

    let internal merge (old: User) (new' : User) =
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

    let Set (user : User) =
        printfn "  Setting user %i '%s'" user.Id user.Name

        if dict.ContainsKey(user.Id) then
            dict.[user.Id] <- merge dict.[user.Id] user
        else
            dict.[user.Id] <- user

    let Print () =
        printfn "Users ="
        Util.PrintDictionary dict

    // Deleted users don't have a visible id. We try to look up by name and assign the first negative id if not found.
    let GuestUserId name =
        let user = dict.Where(fun user -> user.Value.Name = name).FirstOrDefault()

        if user.Key = 0 then
            let rec findAvailableGuestId id = if dict.ContainsKey id then findAvailableGuestId (id - 1) else id
            findAvailableGuestId -1
        else
            user.Key

module Posts =
    let internal dict = new SortedDictionary<int, Post>()

    let internal merge (old: Post) (new' : Post) =
        let _, newSourceTime = new'.Sources |> Map.toList |> List.head
        let previousOfAny = Util.PreviousSourceOfAny old.Sources

        // Posts can only originate from topic sources, so we're only comparing timestamps.
        if newSourceTime < previousOfAny then
            old
        else
            new'

    let Set (post : Post) =
        printfn "  Setting post %i in topic %i by user %i" post.Id post.TopicId post.UserId

        if dict.ContainsKey(post.Id) then
            dict.[post.Id] <- merge dict.[post.Id] post
        else
            dict.[post.Id] <- post

    let Print () =
        printfn "Posts ="
        Util.PrintDictionary dict

module Topics =
    let internal dict = new SortedDictionary<int, Topic>()

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

    let Set (topic : Topic) =
        printfn "  Setting topic %i '%s'" topic.Id topic.Title

        if dict.ContainsKey(topic.Id) then
            dict.[topic.Id] <- merge dict.[topic.Id] topic
        else
            dict.[topic.Id] <- topic

    let Print () =
        printfn "Topics ="
        Util.PrintDictionary dict

module Forums =
    let internal dict = new SortedDictionary<int, Forum>()

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

    let Set (forum : Forum) =
        printfn "  Setting forum %i '%s'" forum.Id forum.Name

        if dict.ContainsKey(forum.Id) then
            dict.[forum.Id] <- merge dict.[forum.Id] forum
        else
            dict.[forum.Id] <- forum

    let Print () =
        printfn "Forums ="
        Util.PrintDictionary dict
