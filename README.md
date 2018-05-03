# UtilityFunctions

The package is a growing collection of handy functions. Since I mainly work on point pattern analysis, many are somehow connected to this topic.

### Installing

You can install UtilityFunctions using the following code:

```r
devtools::install_github(repo = 'mhesselbarth/UtilityFunctions')
``` 

## Examples


```r
pp <- spatstat::rThomas(kappa = 15, scale = 0.25, mu = 5)
sim_envel <- spatstat::envelope(Y = pp, fun = pcf, divisor = 'd')

result <- Plot.Envelope(input = sim_envel, xlab = 'r', ylab = 'pcf(r)')

Save.Function.ggplot(plot = result, filename = 'example_plot.jpeg')
```

### Authors

* **Maximilian H.K. Hesselbarth**

## License

This project is licensed under the GNU3 License - see the [LICENSE](https://github.com/mhesselbarth/UtilityFunctions/blob/master/LICENSE) file for details

## Acknowledgments

Thanks to Daniel Esser for the idea of the simulaten envelope bar plos
