# SNS Comments data

A dataset of 630 German participants in an online experiment. The
experiment investigated the effects of user comments on social network
sites (SNS) on individuals' perceptions of journalistic quality. The
researchers varied the subject of the article (factor 1: 'Copyright
directive' or 'Social housing'), the order of comment presentation
(factor 2: before or after the article) and the valence of the comments
(factor 3: positive or negative).

## Usage

``` r
snscomments
```

## Format

A data frame of 630 observations and 15 variables:

- age:

  Age of the participant

- gender:

  Gender of the participant, either 'female' or 'not female'

- education:

  Level of formal education of the participant, either 'low formal
  education' or 'high formal education'

- need_cognition:

  Index measuring the psychological trait of a person to enjoy thinking,
  calculated from several survey items

- prior_knowledge:

  Index measuring a person's prior knowledge of the presented subject of
  the article, calculated from several survey items

- group:

  Numeric id of the group that the participant was in during the
  experiment

- issue:

  Subject of the article that the participant was given to read, either
  'Copyright directive' or 'Social housing'

- order:

  Order of the comments that the participant was exposed to, either
  'Comments after', 'Comments before', or 'Control group'

- valence:

  Valence of the comments that the participant was exposed to, either
  'Negative', 'Positive', or 'Control group'

- control_group:

  Indicates whether the participant was in the 'Control group' or
  'Experimental group'

- medium_evaluation:

  Index measuring participant's evaluation of the medium's quality,
  calculated from several survey items

- article_evaluation:

  Index measuring participant's evaluation of the article's quality,
  calculated from several survey items

- comments_quality:

  Participant's perception of the quality of the comments

- comments_valence:

  Participant's perception of the valence of the comments

- article_elaboration:

  Participant's measure of how much attention they put in reading the
  article

## Source

<https://osf.io/r867v/>

## Details

This dataset was created from the OSF project: <https://osf.io/r867v/>,
corresponding to the paper: Kümpel, A. S., & Unkel, J. (2020).
Negativity wins at last: How presentation order and valence of user
comments affect perceptions of journalistic quality. Journal of Media
Psychology: Theories, Methods, and Applications, 32(2), 89–99.
[doi:10.1027/1864-1105/a000261](https://doi.org/10.1027/1864-1105/a000261)
