# Incivil Comments Data

A dataset of a preregistered factorial survey experiment with a
nationally representative sample of 964 German online users.
Participants were presented with manipulated user comments that included
statements associated with incivil discourse (such as profanity and
attacks on arguments) and intolerant discourse (such as offensive
stereotyping and violent threats). Participants rated the comments, e.g.
offensiveness, harm to society, and their intention to delete the
comment containing the statement.

## Usage

``` r
incvlcomments
```

## Format

A data frame of 3856 observations nested in 964 participants and 22
variables:

- participant_num:

  Numeric id of the participant

- age:

  Age of the participant

- male:

  Gender of the participant, either 'male' or 'not male'

- high_education:

  Level of formal education of the participant, either 'high formal
  education' or 'low formal education'

- comment_num:

  Numeric id of the comment that the participant was exposed to

- issue:

  The subject of the comment that the participant was exposed to, either
  'Gender', 'Abortion', 'Climate', or 'Migration'

- profanity:

  Whether the comment contained profanities as an indicator of
  incivility

- attacks_argument:

  Whether the comment contained attacks towards arguments as an
  indicator of incivility

- offensive_stereotyping:

  Whether the comment contained offensive stereotypes as an indicator of
  intolerant discourse

- violent_threats:

  Whether the comment contained violent threats as an indicator of
  intolerant discourse

- offensiveness:

  Rate statement whether the comment is being perceived as offensive &
  hostile (Scale from 1 to 7)

- adequacy:

  Rate statement whether the comment is being perceived as necessary &
  accurate (Scale from 1 to 7)

- harm_to_society:

  Rate statement whether the comment is being perceived as harmful to
  society (Scale from 1 to 7)

- deletion_intention:

  Whether the participant wants to delete the comment

- similarity_poster:

  How similar the participant feels to the person who created the post
  (Scale from 1 to 7)

- similarity_group:

  How similar the participant feels to the group of people criticized in
  the post (Scale from 1 to 7)

- attitude_gender:

  Rate agreement with statements on gender policies (Scale from 1 to 7)

- attitude_abortion:

  Rate agreement with statements on abortion (Scale from 1 to 7)

- attitude_migration:

  Rate agreement with statements on migration (Scale from 1 to 7)

- attitude_climate:

  Rate agreement with statements on climate change (Scale from 1 to 7)

- left_right_placement:

  Placement on a political spectrum from left to right (Scale from 1 to
  9)

- freedom_of_speech:

  Rate agreement with statements about the freedom of speech and
  expression (Scale from 1 to 7)

## Source

<https://osf.io/w92vj>

## Details

The dataset was created from the OSF project: [Differential perceptions
of and reactions to incivil and intolerant user
comments](https://osf.io/w92vj), corresponding to the paper: Kümpel, A.
S., Unkel, J (2023). Differential perceptions of and reactions to
incivil and intolerant user comments, Journal of Computer-Mediated
Communication, Volume 28, Issue 4, https://doi.org/10.1093/jcmc/zmad018
