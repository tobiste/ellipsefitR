
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ellipsefitR

<!-- badges: start -->

<!-- badges: end -->

`ellipsefit` is an R package for interactive digitization and
quantitative analysis of ellipses from images. It was developed
primarily for geological applications such as strain analysis, clast
shape analysis, porphyroclasts, reduction spots, ooids, fossils, and
other approximately elliptical objects, but can be applied to any image
containing elliptical features.

The package provides tools to

- import photographs,
- correct perspective distortion,
- rotate images,
- calibrate image scale,
- interactively digitize multiple ellipses,
- fit ellipses using a least-squares approach,
- quantify ellipse geometry and fitting uncertainty,
- export all measurements to a data frame.

## Installation

``` r
# install.packages("remotes")

remotes::install_github("YOUR_GITHUB_USERNAME/ellipsefitR")
```

## Proposed workflow

The recommended workflow is

    Open image
          │
          ▼
    Perspective correction (optional)
          │
          ▼
    Image rotation (optional)
          │
          ▼
    Scale calibration (optional)
          │
          ▼
    Digitize ellipses
          │
          ▼
    Export measurements

Perspective correction should always be performed before scale
calibration because perspective transformations alter image geometry.

------------------------------------------------------------------------

## Example

Load the package

``` r
library(ellipsefitR)
```

### 1. Open an image

``` r
img <- imageOpen("example.jpg")
```

Display the image

``` r
plot(img)
```

------------------------------------------------------------------------

### 2. Correct perspective (optional)

If the photograph was taken obliquely,

``` r
img <- perspectiveCorrect(img)
```

Click the four corners of a rectangular object in clockwise order.

------------------------------------------------------------------------

### 3. Rotate image (optional)

If the image is tilted,

``` r
img <- rotateImage(img)
```

Click two points defining a line that should become horizontal.

------------------------------------------------------------------------

### 4. Calibrate scale (optional)

If a scale bar is present,

``` r
img <- calibrateScale(
    img,
    length = 10,
    units = "mm"
)
```

Click both ends of the scale bar.

All subsequent measurements will be reported in millimetres.

------------------------------------------------------------------------

### 5. Digitize ellipses

``` r
fits <- imageEllipse(img)
```

For each ellipse,

- left-click around the boundary,
- finish with a right mouse click,
- choose whether another ellipse should be digitized.

All previously digitized ellipses remain visible.

------------------------------------------------------------------------

### 6. Plot the results

``` r
plot(fits)
```

The plot shows

- original image,
- digitized points,
- fitted ellipses,
- ellipse numbers.

------------------------------------------------------------------------

### 7. Export measurements

Convert all ellipse measurements into a data frame

``` r
df <- as.data.frame(fits)

head(df)
```

Typical output

|  id | major | minor | angle | eccentricity | area | rmse |
|----:|------:|------:|------:|-------------:|-----:|-----:|
|   1 |  12.5 |   8.1 |  34.2 |         0.76 | 79.5 | 0.15 |
|   2 |  10.7 |   6.4 |  21.8 |         0.80 | 53.8 | 0.12 |

------------------------------------------------------------------------

## Individual ellipse

Access one ellipse

``` r
e <- fits$ellipses[[1]]
```

Inspect its properties

``` r
summary(e)
```

or

``` r
plot(e)
```

------------------------------------------------------------------------

## Available measurements

Each fitted ellipse contains

- centre coordinates
- major axis
- minor axis
- orientation
- eccentricity
- aspect ratio
- area
- residuals
- RMSE
- original digitized points

If the image was calibrated, dimensions are additionally reported in
calibrated units.

------------------------------------------------------------------------

## Typical geological applications

- strain analysis
- Rf/φ method
- clast shape analysis
- porphyroclast analysis
- reduction spots
- ooids
- vesicles
- fossils
- concretions
- boudins
- mineral grains
- any approximately elliptical object

------------------------------------------------------------------------

## Citation

If you use **ellipsefitR** in published work, please cite

> *Citation information will be added upon publication.*
