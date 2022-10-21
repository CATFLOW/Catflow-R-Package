library(Catflow)


# build an example
north <- seq(1, 11, length=20)
east <- seq(2, 8, length=20)
elev <- approx(c(8,5), n=20)$y + sin((0:19)/2)/5
slopewidth <- rep(1, 20)

# create a slope
simple.slope <- list(
    xh=north,
    yh=east,
    zh=elev,
    bh=slopewidth,
    tot.area = 12,
    htyp = 1,
    dyy = 2,
    xsi=seq(0,1, length=21),
    eta=seq(0, 1, length=11),
    out.file="out/test.geo"
)

# make
test.geom <- make.geometry(simple.slope)