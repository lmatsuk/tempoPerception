# tempoPerception
Contains raw data and analysis code for the paper "Body Height, Age, Musical and Dance Sophistication Predict Tempo Perception in Music", currently (Feb '26) under review.

**Abstract**
The perception of tempo is an important aspect of music cognition. But tempo is usually associated with visible movement in physical space, which acoustical sig-nals do not provide. Therefore, we propose that body movement tempo bridges the gap between physical movement tempo and musical tempo. As individual move-ment tempi differ, we expected that the perception of tempo must differ inter-individually as well. Participants listened to a set of the same musical stimulus in twenty different tempi (40–135 bpm) and rated their perceived tempo on a 5-point scale. Results show that factors influencing movement tempo in musical and non-musical contexts (body height, age, musical and dance expertise) also influence perceived musical tempo. This effect is dependent on the tempo range in which it occurs, mainly appearing at the upper and lower ends of the tempo scale used.

In this repository, these files can be found:

1. The raw questionnaire data, downloaded from the questionnaire platform: SoSciSurvey.de
2. A table containing the data constituting the independent (body height + weight, age, musical + dance sophistication) and dependent (perceived tempo per stimulus) variables in wide format (one row per participant).
3. The code in R (Version 20.24.12.1+563) used for analysis containing the formatting into long format, several steps to prepocess the data, the statistical analyses using a linear mixed model (including assumption checks) and visualization.
