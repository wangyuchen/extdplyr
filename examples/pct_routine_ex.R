data(esoph)
esoph
pct_routine(esoph, agegp, alcgp)
pct_routine(esoph, agegp, alcgp, wt = ncases)

# Use SE
pct_routine_(esoph, c("agegp", "alcgp"), wt = quote(ncases))
pct_routine_(esoph, c("agegp", "alcgp"), wt = "ncases")

# Calculate one level up
pct_routine(esoph, agegp, alcgp, margin = 2)
# Crate new grouping variables
pct_routine(esoph, agegp, low_alcgp = alcgp %in% c("0-39g/day", "40-79"))


# This examples shows how rebase works
if (require(dplyr)) {
  iris %>%
    mutate(random_missing = ifelse(rnorm(n()) > 0, NA, round(Sepal.Length))) %>%
    group_by(Species, random_missing) %>%
    tally_pct(wt = Sepal.Width, rebase = TRUE)
}
