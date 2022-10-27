# Solving the climate crisis with Monads

### Talk outline

(Less of a talk outline and more of just a random collection of notes from my
 notebook.)

```
f :: a -> b
```

- Fundamental mistake: We can _always_ convert an "a" into a "b".

- Reality: Converting a's has many down-stream impacts, least of which is your
  ability to continue to perform this conversion.

- Consider:

  Step 1:

  ```
  f :: (IsOkay e) => a -> e b
  ```

  or maybe:

  ```
  ff :: Env (Supply As)
  ```

  - Somehow model a's getting expensive/running out.

- Then, Step 2:

  ```
  f' :: a -> (b, TripleBottomLine)
  ```

- Step 3: No unlimited source of a's?
- Step 4: Everything is interconnected

  ```
  f'' :: Env (a, b) -> Env (a, b)
  ```

  Problem: Abstraction is entirely gone! May as well just live in IO.

- Next:
  - How to build adaptable businesses?
  - i.e. `f` should very over time.

- Terrible ideas:
  - `f :: a -> b -> ((a, b), b)`
  - Emits a new function each time? (No, it's equivalent to not doing that.)

### Things to not forget

- What is `f :: a -> b`?
  - Yes, it's a function.
  - But, I consider it to also be a business.

- Some kind of cool animation showing the world blowing up.
- At the start:
  - Maybe the title sounds too ambitious. So okay, let's make it more
  ambitious:
    - Solving _everything_ with context.
    - Or less ambitious:
      - _Analysing_ the climate crisis
      - But _understanding_ allows us to _solve_

- Rosa Luxemburgh;
  - Surplus
  - Capitalism must colonise to exist

- Adrienne Buller
  - Working means by definition you don't collect all your value
  - It goes as surplus for the owners
  - Implication: That means colonising new places; i.e. infinities.

- Era of the end of abstraction
  - Can _no longer_ decompose; focus only on sub-components; only think
  locally; ?!?!!?!?!
    - Extremely stressful?

- Emissions
  - Scope 1, 2, 3 emissions
    - 3 - Ill-posed
    - All your downstream suppliers
    - If two people focus on scope 3, they could be focusing on the same thing
      - Feature?
      - But is it _surfaced_ ; i.e. who are they people that, by fixing their
        scope 1 emissions, make the biggest impact?


#### Random links

- <https://github.com/buildingdata/earth3d-radiation-threejs>
  - <https://github.com/silky/ExplodingObjects>

  Idea: Use this planet; then explode it slightly, then crush up small things.

- <https://github.com/silky/threex.planets/blob/master/examples/earth.html>

- <https://carbontracker.org/>
  - <https://www.linkedin.com/pulse/lifegoals-lewis-jenkins/?trackingId=foJIXnopOVGBiLkjSbq26A%3D%3D>
  - "Out of more than 130 industrial companies that account for the bulk of pollution, 98 per cent did not provide evidence that their 2021 financial statements had taken into account the effects of climate-related matters..."

  - At the same the recent Pay for Climate Performance report "Grades 47 of the largest companies in the US on their use of climate related pay incentives... A whopping 89% of assessed companies received D or F grades for failing to include a quantitate or *any* type of climate related incentive"

- <https://www.un.org/sg/en/content/sg/statement/2022-09-20/secretary-generals-address-the-general-assembly-trilingual-delivered-follows-scroll-further-down-for-all-english-and-all-french>

- <https://doughnuteconomics.org/stories/188>
