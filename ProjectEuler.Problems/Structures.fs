module Structures

type GraphNode(value : int) = 
    let mutable _nodes = []
    let _value = value

    member this.Value with get () = _value

    member this.AddChild(value : int) = 
        _nodes <- new GraphNode(value) :: _nodes
    
    member this.AddChild(node : GraphNode) = _nodes <- node :: _nodes

    member this.Exhaustive() : int = 
        if _nodes.Length = 0 then _value
        else _value + (_nodes |> Seq.map(fun f -> f.Exhaustive()) |> Seq.max)


type Graph(input : int array array) = 
    
    let rec buildGraph (slot : int, a : int array array, parents : GraphNode array) = 
        let thisLevel = a.[slot]    

        let nodes = thisLevel |> Seq.map(fun f -> new GraphNode(f)) |> Seq.toArray

        nodes
        |> Seq.pairwise 
        |> Seq.mapi(fun index pair -> (index, pair)) 
        |> Seq.iter(fun (index, pair) -> 
                        ignore(parents.[index].AddChild(fst pair))
                        ignore(parents.[index].AddChild(snd pair)))

        if slot + 1 < a.Length then Seq.toList(nodes) @ buildGraph(slot + 1, a, nodes)
        else Seq.toList(nodes)

    let root = buildGraph(0, input, Array.empty) |> Seq.head

    member this.Max() = root.Exhaustive()
        
                                       
        

