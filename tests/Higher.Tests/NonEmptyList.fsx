#r "bin/release/Higher.Core.dll"

open Higher.Core

type NonEmptyList<'a> = App2<Cofree, Option, 'a>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NonEmptyList =
  
  let singleton (a:'a) : NonEmptyList<'a> =
    Cofree.Cofree (a, None |> Option.Inj) |> Cofree.Inj

  let cons (a:'a) (l:NonEmptyList<'a>) : NonEmptyList<'a> =
    Cofree.Cofree (a, Option.Inj (Some (Cofree.Prj l))) |> Cofree.Inj
  
  let head (l:NonEmptyList<'a>) : 'a =
    Cofree.head (Cofree.Prj l)

  let tail (l:NonEmptyList<'a>) : NonEmptyList<'a> option =
    Cofree.tail (Cofree.Prj l) |> Option.Prj |> Option.map (Cofree.Inj)

  let ana (f:'A -> 'B) (g:'A -> option<'A>) (a:'A) : NonEmptyList<'B> =
    Cofree.ana (new OptionMonad()) f (g >> Option.Inj) a |> Cofree.Inj

  let append (l1:NonEmptyList<'a>) (l2:NonEmptyList<'a>) : NonEmptyList<'a> =
    ana 
      (fun l -> let (Cofree(a,_)) = Cofree.Prj l in a)
      (fun l -> 
        let (Cofree(_,tl)) = Cofree.Prj l
        match Option.Prj tl with
        | Some tl -> Some (Cofree.Inj tl)
        | None -> Some l2)
      l1

  let rec foldMap (M:Semigroup<'M>) (f:'a -> 'M) (l:NonEmptyList<'a>) : 'M =
    let (Cofree(a,tl)) = Cofree.Prj l
    match Option.Prj tl with
    | Some tl -> M.Append (f a) (foldMap M f (Cofree.Inj tl))      
    | None -> f a

  let length<'a> : NonEmptyList<'a> -> int =
    foldMap (IntSumMonoid()) (fun _ -> 1)    

  let inline sum (l:NonEmptyList<'a>) : 'a =
    foldMap (Semigroup.ofAppend (+)) id l

  let toList (l:NonEmptyList<'a>) : 'a list =
    Cofree.toList (Cofree.Prj l)
