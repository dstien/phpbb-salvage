module Types

open System

type Config = {
    DataDir   : string
    Verbosity : int
    Error     : bool
}

type SourceType =
    | Index
    | Forum
    | Topic
    | Memberlist
    | Profile

type User = {
    Id         : int
    Name       : string
    Rank       : string
    CustomRank : string
    JoinDate   : DateTime
    PostCount  : int
    CanEmail   : bool
    Avatar     : string option
    Location   : string option
    Homepage   : string option
    Occupation : string option
    Interests  : string option
    XboxTag    : string option
    AIM        : string option
    YM         : string option
    MSN        : string option
    ICQ        : string option
    Signature  : string option
    Sources    : Map<SourceType, DateTime>
} with
    static member Stub id name rank timestamp =
        {
            Id         = id
            Name       = name
            Rank       = rank
            CustomRank = ""
            JoinDate   = DateTime.MinValue
            PostCount  = 0
            CanEmail   = false
            Avatar     = None
            Location   = None
            Homepage   = None
            Occupation = None
            Interests  = None
            XboxTag    = None
            AIM        = None
            YM         = None
            MSN        = None
            ICQ        = None
            Signature  = None
            Sources    = Map.empty.Add(SourceType.Index, timestamp)
        }

type Edited = {
    User  : string
    Count : int
    Last  : DateTime
}

type Post = {
    Id        : int
    Timestamp : DateTime
    UserId    : int
    TopicId   : int
    Title     : string
    Content   : string
    Edited    : Edited option
    Sources   : Map<SourceType, DateTime>
}

type PollOption = {
    Text  : string
    Votes : int
}

type Poll = {
    Question : string
    Options  : PollOption list
    Votes    : int
}

type Topic = {
    Id           : int
    ForumId      : int
    UserId       : int
    Title        : string
    Locked       : bool
    Announcement : bool
    Sticky       : bool
    Poll         : Poll option
    Replies      : int
    Views        : int
    Sources      : Map<SourceType, DateTime>
}

type Forum = {
    Id          : int
    Name        : string
    Description : string
    Moderators  : int list
    Order       : int
    TopicCount  : int
    PostCount   : int
    Sources     : Map<SourceType, DateTime>
} with
    static member Stub id name timestamp =
            {
                Id          = id
                Name        = name
                Description = ""
                Moderators  = []
                Order       = -1
                TopicCount  = -1
                PostCount   = -1
                Sources     = Map.empty.Add(SourceType.Topic, timestamp)
            }
