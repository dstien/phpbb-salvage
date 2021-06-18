open System
open System.Linq

open Collections

let ReadFiles (dir : string) (pattern : string) (parser : string -> unit) =
    IO.DirectoryInfo(dir)
        .GetFiles()
        .Where(fun f -> f.Name.Contains(pattern))
        .Where(fun f -> not (f.Name.Contains("login.php")))
        .OrderByDescending(fun f -> f.CreationTime)
        .Select(fun f-> f.FullName)
        .ToArray()
    |> Array.iter parser

let ReadDir dir =
    ReadFiles dir "viewtopic.php" Parsers.Topic.Parse
    ReadFiles dir "viewforum.php" Parsers.Forum.Parse
    ReadFiles dir "memberlist.php" Parsers.Memberlist.Parse
    ReadFiles dir "profile.php" Parsers.User.Parse

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: %s datadir" System.AppDomain.CurrentDomain.FriendlyName
        1
    else
        printfn "Reading data directory \"%s\"..." argv.[0]
        ReadDir argv.[0]

        Users.Print()
        Forums.Print()
        Topics.Print ()
        Posts.Print ()

        0
