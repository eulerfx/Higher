namespace Higher.Core

type Yoneda<'F, 'A> private (func : Functor<'F>) =  
  member self.Map<'B>(f:'A -> 'B) (fa : App<'F, 'A>) : App<'F, 'B> = 
    func.Map f fa
  static member toYoneda (func : Functor<'F>) =
    Yoneda<'F, 'A>(func)
  static member fromYoneda (yo : Yoneda<'F, 'A>) : App<'F, 'A> -> App<'F, 'A> = 
    yo.Map id


type CoYoneda<'F, 'A> private (fa : App<'F, 'A>, f : 'A -> 'A) =
  member self.f = f
  member self.fa = fa  
  static member toCoYoneda (fa : App<'F, 'A>) = 
    CoYoneda<'F, 'A>(fa, id)
  static member fromCoYoneda (func : Functor<'F>) (coyo : CoYoneda<'F, 'A>) : App<'F, 'A> =
    func.Map (coyo.f) (coyo.fa)
    