# TODO
- cleanup naming of parameters
- Add tests for core functionality
  - vectors created within functions do what we expect (shouldnt be assertions)
  - seed works
- document assertions better

# Possible future features
- `add_agents(df1, df2)` to combine `agents` data.frames 
- `as.agent(df)` to convert a data.frame to an `agents` object, running all necessary assertions
- Add `obj_effect` as a paremter to `create_studies` to directly pass an effect sizes instead of always simulating it


assertions that should be in tests:
```r
  assert_numeric(effect_size, len = 1, finite = TRUE, any.missing = FALSE)
  assert_integerish(fault_ind, len = study[["N"]], lower = 0, upper = 1, any.missing = FALSE)
  assert_numeric(error_sizes, len = study[["N"]], finite = TRUE, any.missing = FALSE)
  assert_numeric(obs_effect_sizes, len = study[["N"]], finite = TRUE, any.missing = FALSE)

```
