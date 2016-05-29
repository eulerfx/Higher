namespace Higher.Core

/// Foldable
[<AbstractClass>]
type Foldable<'T> () =
  abstract FoldMap<'A, 'M> : Monoid<'M> -> ('A -> 'M) -> App<'T, 'A> -> 'M
  member self.Fold<'M> (M:Monoid<'M>) : App<'T, 'M> -> 'M =
    self.FoldMap M id
  member self.Foldr<'A, 'B> (f:'A -> 'B -> 'B) (b:'B) : App<'T, 'A> -> 'B =    
    self.FoldMap (new EndoMonoid<_>()) f >> (|>) b
  member self.Foldl<'A, 'B> (f:'B -> 'A -> 'B) (b:'B) : App<'T, 'A> -> 'B =    
    let swap f a b = f b a in
    self.FoldMap (new EndoMonoid<_>()) (swap f) >> (|>) b
  member self.ToList<'A> (ta:App<'T, 'A>) : 'A list =
    self.FoldMap (new ListMonoid<_>()) (fun a -> [a]) ta