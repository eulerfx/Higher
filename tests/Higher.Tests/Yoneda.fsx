#r "bin/release/Higher.Core.dll"

open Higher.Core

type Eq<'a, 'b> = 
  private 
  | Eq

module Is =
  
  let refl<'a> : Eq<'a, 'a> =
    Eq
  
  let coerceA (eq:Eq<'a, 'b>) (a:'a) : 'b =    
    (box a) :?> 'b

  let coerceB (eq:Eq<'a, 'b>) (b:'b) : 'a =
    (box b) :?> 'a
//
//  let subst (eq:Eq<'a, 'b>) (fa:App<'F, 'a>) : App<'F, 'b> =
//    fa




type Leib<'a, 'b> =
  abstract member Subst<'F> : App<'F, 'a> -> App<'F, 'b>

module Leib =
  
  let refl<'a> : Leib<'a, 'a> =
    { new Leib<_, _> with
        override __.Subst<'F> (fa:App<'F, 'a>) = fa }






type Yoneda<'F, 'a> =
  abstract member Map<'b> : ('a -> 'b) -> App<'F, 'b>

module Yoneda =
  
  let toYo (F:Functor<'F>) (fa:App<'F, 'a>) : Yoneda<'F, 'a> =
    { new Yoneda<_, _> with
        member __.Map f = F.Map f fa }

  let fromYo (yo:Yoneda<'F, 'a>) : App<'F, 'a> =
    yo.Map id





type CoYoneda<'F, 'a, 'b> =
  abstract member CoYo : ('b -> 'a) * App<'F, 'b>

type CoYoneda<'F, 'a> = CoYoneda<'F, 'a, 'a>

module CoYoneda =
  
  let toCoYo (fa:App<'F, 'a>) : CoYoneda<'F, 'a> =
    { new CoYoneda<'F, 'a> with
        override __.CoYo : ('a -> 'a) * App<'F, 'a> =
          id,fa }

  let fromCoYo (F:Functor<'F>) (coyo:CoYoneda<'F, 'a>) : App<'F, 'a> =
    let f,fa = coyo.CoYo in F.Map f fa




