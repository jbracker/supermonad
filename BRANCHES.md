 
# Description of different branches available

## `master`

Current development.

## `use-associated-type-synonyms`

Early version of the naive plugin, where we tried to use associated type
synonyms instead of the functional dependency in the `Bind` type class.
This was discontinued, because some of the instances we wanted to define
caused GHCs type checker to loop, whereas they did not cause that behaviour 
when using functional dependencies.

The example for this behaviour can be seen in the following file:
[MainSupermonad.hs](https://github.com/jbracker/supermonad-plugin/blob/use-associated-type-synonyms/examples/effect/MainSupermonad.hs).

## `use-exponential-solving`

State of the plugin when we replaced the naive solving algorithm with 
a more general algorithm (that has exponential runtime). This is before
we decided to try a new approach.

## Terms

* **Naive solving algorithm**: This solving algorithm just defaulted every
  `Return` constraint to `Identity` and made a choice which of the 
  two overlapping instances `Bind m Identity m` and `Bind Identity m m` should
  be used when there was ambiguity. Algorithm required many type annotations 
  and therefore was replaced with the exponential solving algorithm. 
  Its remnants can still be seen in the `use-exponential-solving` 
  branch (module `Plugin`).

* **Exponential solving algorithm**: This solving algorithm was more 
  sophisticated and actually looked at all associations between ambiguous 
  variables and type constructors (hence exponential behaviour). It worked
  better then the naive approach, but once there was more then one possible
  solution for a set of constraints we did not know how to make a decision
  for either of them.
  
  A simple example of this problem can be seen by
  looking at the constraint `Bind Maybe m Maybe` where `m` is ambiguous.
  Possible type constructors for `m` are `Identity` and `Maybe`. As long
  as `m` is not involved with any other constraints the choice does not matter,
  but there is also no convincing argument for a preference of either of them.
  
  This problem occurs mainly, because we allow `Bind` instances that lift 
  one supermonad into another, e.g., `Identity` into any supermonad 
  or `Maybe` into `List`. 
  
  Therefore, we decided to take a different approach (the current one).
  
