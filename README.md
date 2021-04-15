<!-- Add a project state badge
See https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin. -->

# FAIBCompiler

<!-- badges: start -->

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
<!-- badges: end -->

FAIBCompiler comtains the functions to run VRI and PSP compilers. The
compilers

-   load data from FAIB oracle ground sample databases using package
    **FAIBOracle**,
-   manipulate tree/stand-level tables, calculate tree/stand-level
    attributes such as basal area and volume using **FAIBBase**,
-   save and archive them.

### Features

Cluster id changed from proj_id-samp_no-samp_type-visit to
site_identifier-samp_type-visit, because EFR no longer uses proj_id and
sample number as an unique spatial point. It uses site identifier now.
The change was made on Nov. 24, 2020. The previous versions of
VRICompiler and ISMCCompiler bave been freezed in VGISCompiler branch.

### Installation

### Usage

#### Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

### Project Status

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/FAIBCompiler/issues/).

### How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2019 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

*This project was created using the
[bcgovr](https://github.com/bcgov/bcgovr) package.*
