test_that("base functionality works", {
  expect_equal(
    as.character(table1(~x, data.frame(x=c(1,1,2)))),
    "<table class=\"Rtable1\">\n<thead>\n<tr>\n<th class='rowlabel firstrow lastrow'></th>\n<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=3)</span></span></th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>x</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>1.33 (0.577)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>1.00 [1.00, 2.00]</td>\n</tr>\n</tbody>\n</table>\n"
  )
})

test_that("custom missing works", {
  expect_equal(
    as.character(table1(~x, data.frame(x=c(1,1,NA_integer_)))),
    "<table class=\"Rtable1\">\n<thead>\n<tr>\n<th class='rowlabel firstrow lastrow'></th>\n<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=3)</span></span></th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>x</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>1.00 (0)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Median [Min, Max]</td>\n<td>1.00 [1.00, 1.00]</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Missing</td>\n<td class='lastrow'>1 (33.3%)</td>\n</tr>\n</tbody>\n</table>\n"
  )
  
  render.missing.custom <- function(x, name, ...) {
    ## Use a switch to customize MISSING label based on Variable name
    missing_label <- switch(
      name,
      x = "x missing",
      y = "y missing",
      "unknown missing"
    )
    with(stats.apply.rounding(stats.default(is.na(x), ...), ...)$Yes,
         c(setNames(sprintf("%s (%s%%)", FREQ, PCT), missing_label)))
  }
  
  expect_equal(
    as.character(
      table1(
        ~x + y + z, 
        data.frame(x=c(1,1,NA_integer_),y=c(1,1,NA_integer_),z=c(1,1,NA_integer_)),
        render.missing = render.missing.custom
      )
    ),
    "<table class=\"Rtable1\">\n<thead>\n<tr>\n<th class='rowlabel firstrow lastrow'></th>\n<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=3)</span></span></th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>x</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>1.00 (0)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Median [Min, Max]</td>\n<td>1.00 [1.00, 1.00]</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>x missing</td>\n<td class='lastrow'>1 (33.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>y</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>1.00 (0)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Median [Min, Max]</td>\n<td>1.00 [1.00, 1.00]</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>y missing</td>\n<td class='lastrow'>1 (33.3%)</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>z</span></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>1.00 (0)</td>\n</tr>\n<tr>\n<td class='rowlabel'>Median [Min, Max]</td>\n<td>1.00 [1.00, 1.00]</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>unknown missing</td>\n<td class='lastrow'>1 (33.3%)</td>\n</tr>\n</tbody>\n</table>\n"
  )
})

