Lifestreams
===========

Lifestreams is a data analysis stack designed to runs on top of Ohmage (https://github.com/ohmage/). The Lifestreams stack consists of four layers: 1) feature extraction and aggregation, 2) feature selection, 3) inference and 4) visualization.
Each layer consists of modular plugins or building blocks that can process the data provided by the lower layer, and send the result to the next layer up.
An ohmage project owner can select a suitable set of Lifestreams modules or plugin new modules to pocess the data.
Most modules are currentlly written in R scripts, and some in Java. More documentation and APIs are comming soon in a couple of months.
