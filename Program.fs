
open System
// https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/fsharp-collection-types
open System.Collections.Generic

type Tag = {
    ID: Guid
    Name: String
    Description: String
    Relations: Set<Guid>
}

type Tags = Set<Guid>
type TagLookup = Dictionary<Guid,Tag>
type SuggestionLookup = Dictionary<string,Set<Guid>>

type TagService() =

    let lookup = new TagLookup()
    let mutable tags = Set.empty
    let suggestions = new SuggestionLookup()

    member __.Tags = tags
    member __.Lookup = lookup
    member __.Suggestions = suggestions

    member __.AddTag (t:Tag) =
        // O(log(N))
        tags <- Set.add t.ID __.Tags
        // O(1)
        lookup.[t.ID] <- t
        // O(n^2)
        // This could be backgrounded
        __.IndexRelationships()
        __.IndexSuggestions(t)

    member __.GetTag (ID:Guid) =
        // O(1)
        try
            lookup.[ID]
        with _ -> failwith $"Tag {ID} does not exist."

    member __.RemoveTag (ID:Guid) =
        try
            // O(1)
            do lookup.Remove(ID) |> ignore
            // O(log(N))
            tags <- Set.remove ID tags
            // O(n^2)
            // This could be backgrounded
            __.IndexRelationships()
        with _ -> failwith $"Could not remove Tag:{ID}"

    member __.CreateTag (name:string) (description:string) =
        {
            Name = name;
            Description = description;
            Relations = Set.empty;
            ID = Guid.NewGuid();
        }
    
    member __.UpdateTag (t:Tag) =
        try 
            lookup.[t.ID] <- t
        with _ -> failwith $"Tag:{t.ID} could not be updated."
    
    member __.AddRelationships (t:Tag) (relatesTo: Guid list) =
        // O(M*log(N))
        { t with Relations = Set.union t.Relations (Set.ofList relatesTo) }

    member __.RemoveRelationships (t:Tag) (unrelatedTo: Guid list) =
        // O(M*log(N))
        { t with Relations = Set.difference t.Relations (Set.ofList unrelatedTo) }

    member __.Print () =
        for tag in lookup do
            printfn "Tag %A : %A" tag.Value.Name tag.Value
    
    member __.PrintRelationships () =
        for tag in lookup do
            printfn $"Relationships for {tag.Value.Name}: {tag.Key} <-> {tag.Value.Relations}"

    member __.Decompose (t:Tag) =
        let splitOn = [|","; "&";" ";";";":"|]
        let nameInfo = 
            t.Name.Split(splitOn, StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun s -> s.Trim().ToLowerInvariant())
            |> Set.ofArray

        let descInfo =
            // O(n*3):
            // O(n)
            t.Description.Split(splitOn, StringSplitOptions.RemoveEmptyEntries)
            // O(n)
            |> Array.map (fun s -> s.ToLowerInvariant())
            // O(n)
            |> Set.ofArray
       
        Set.union nameInfo descInfo

    member __.IsRelated (t1:Tag) (t2:Tag) =
        let relationship = Set.intersect (__.Decompose t1) (__.Decompose t2)
        Set.count relationship > 0

    member __.IndexSuggestions (t:Tag) =
        __.Decompose t 
        |> Set.toList
        |> List.map (fun w -> w.Trim().ToLowerInvariant())
        |> List.distinct
        |> List.iter (fun w -> 
            let assoc = Set.add t.ID  Set.empty
            if suggestions.ContainsKey w
            then suggestions.[w] <- Set.union suggestions.[w] assoc
            else suggestions.[w] <- assoc)

    member __.PrintSuggestions () =
        for suggestion in suggestions do
            let tags = 
                Set.toList suggestion.Value
                |> List.map __.GetTag
            for t in tags do
                printfn $"{suggestion.Key} <--> relates to <--> ({t.Name} - {t.Description})"

    member __.Suggest (word:string) =
        let cleaned = word.Trim().ToLowerInvariant()
        match suggestions.ContainsKey cleaned with
        | true ->
            Set.toList suggestions.[cleaned]
            |> List.map (fun g -> __.GetTag g)
        | false -> []
    
    member __.SuggestForTag (t:Tag) =
        __.Decompose t
        |> Set.toList
        |> List.collect __.Suggest
        |> List.distinct
        |> List.filter (fun suggestion -> suggestion.ID <> t.ID)

    // O=n^2 where n = tags
    // Should be background thread on concurrent dict
    member __.IndexRelationships () =
        let tags = Set.toArray tags
        for i = 0 to (tags.Length - 1) do
            let tag1 = __.GetTag tags.[i]
            for j = 0 to (tags.Length - 1) do 
                let tag2 = __.GetTag tags.[j]
                if __.IsRelated tag1 tag2 && tag1.ID <> tag2.ID
                then
                    __.UpdateTag (__.AddRelationships tag1 [tag2.ID])
                    __.UpdateTag (__.AddRelationships tag2 [tag1.ID])

let tagservice = new TagService()
let cookingTag = tagservice.CreateTag "COOKING" "cuisine, travel, writing, recipe, social MEDIA"
[ 
    tagservice.CreateTag "Blogging" "writing, social media, communications";
    tagservice.CreateTag "BLOGGING" "spokenword, writing"
    tagservice.CreateTag "Medicine" "healthcare, disability, health, HEALTHCARE, doctor";
    tagservice.CreateTag "Diversity" "gender, race, socioeconomic, age, disability";
    tagservice.CreateTag "Poetry" "writing, art, creativity, spoken word";
    cookingTag;
]
|> List.iter tagservice.AddTag

tagservice.Print()
printfn "==================================================================="
tagservice.PrintRelationships()
printfn "==================================================================="
tagservice.PrintSuggestions()
printfn "==================================================================="

printfn "Now asking for suggestion for COOKING..."
printfn $"May I suggest... {tagservice.SuggestForTag cookingTag}"



