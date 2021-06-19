module Util

open System
open System.Text.RegularExpressions
open FSharp.Data

open Types

// Parse config from command line arguments.
let rec ParseArgs argv (config : Config) =
    match argv with
    | "-v"::xs -> ParseArgs xs { config with Verbosity = config.Verbosity + 1 }
    | "-q"::xs -> ParseArgs xs { config with Verbosity = 0 }
    | "-f"::xs ->
        // Set file input or error if input has already been set.
        match xs with
        | typ::file::xss ->
            match typ.ToLower() with
            | "index"      -> ParseArgs xss { config with Input = Some (Input.File (SourceType.Index,      file)) }
            | "forum"      -> ParseArgs xss { config with Input = Some (Input.File (SourceType.Forum,      file)) }
            | "topic"      -> ParseArgs xss { config with Input = Some (Input.File (SourceType.Topic,      file)) }
            | "memberlist" -> ParseArgs xss { config with Input = Some (Input.File (SourceType.Memberlist, file)) }
            | "profile"    -> ParseArgs xss { config with Input = Some (Input.File (SourceType.Profile,    file)) }
            | _ -> { config with Error = true }
        | _ -> { config with Error = true }
    | "-?"::xs -> { config with Error = true }
    | str::xs ->
        // Set directory input or error if input has already been set.
        match config.Input with
        | None -> ParseArgs xs { config with Input = Some (Input.Directory str) }
        | _ -> { config with Error = true }
    | [] ->
        // Reached end. Error if input wasn't set.
        match config.Input with
        | None -> { config with Error = true }
        | _ -> config

// Read file into HtmlDocument with timestamp.
let ReadFile (config : Config) (filename : string) =
    let timestamp = IO.File.GetCreationTime(filename)

    // Read entire file and remove all newlines. phpbb have inserted <br/> for every newline in post bodies which FSharp.Data substitutes back to newline.
    let src = IO.File.ReadAllText(filename).Replace("\n", "")
    let doc = HtmlDocument.Load(new IO.StringReader(src))

    if config.Verbosity > 0 then
        printfn "Read %s (%s)" filename (timestamp.ToString())

    (doc, timestamp)

// Print items in a sorted dictionary one by one.
let PrintDictionary (dict : Collections.Generic.SortedDictionary<int, 'T>) =
    let mutable em = dict.GetEnumerator()
    while em.MoveNext() do
        printfn "%A" em.Current.Value

// Get a numeric field value from query string.
let NumericQueryField (link : HtmlNode) (field : string) =
    Int32.Parse(Regex.Match(link.AttributeValue("href"), @"(\?|&)" + field + @"=(\d+)").Groups.[2].Value)

// Parse forum timestamp string relative to source time.
let ParseForumTimestamp (text : string) (sourceTime : DateTime) =
    let formatDate (date : DateTime) = date.ToUniversalTime().ToString("dd MMM yyyy")

    let adjustedText =
        match text with
        | t when t.StartsWith("Today") -> text.Replace("Today", formatDate sourceTime)
        | t when t.StartsWith("Yesterday") -> text.Replace("Yesterday", formatDate (sourceTime.AddDays(-1.0)))
        | t -> t

    DateTime.Parse(adjustedText)

// Get the previous source by date, regardless of type.
let PreviousSourceOfAny (sources : Map<SourceType, DateTime>) =
    sources
    |> Map.toList
    |> List.map (fun s -> snd s)
    |> List.sortDescending
    |> List.head

// Get the previous source of given types, default to DateTime.MinValue.
let PreviousSourceOfTypes (sources : Map<SourceType, DateTime>) (types : SourceType list) =
    sources
    |> Map.filter (fun s _ -> types |> List.contains s)
    |> Map.toList
    |> List.map (fun s -> snd s)
    |> List.sortDescending
    |> List.tryHead
    |> Option.defaultValue DateTime.MinValue
