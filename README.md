# Integrated data visualisation tools: Automated analysis of predation events between lobsters and sea urchins

## Context
This research project focuses on understanding the predation behavior between southern rock lobsters and sea urchins in Tasmanian waters, which are experiencing significant warming and species redistribution. The project tested the relative predation of urchins by the southern rock lobster. Data collection + Science by @jesmith5 University of Tasmania and Data Science by @slopezmarcano from FishiD-Griffith University, Australia.

## Description
This dashboard is designed to allow users to review the predation events that have been identified by the Lobster-Urchin project. The dashboard is built using the R package `shiny`. 

## Data
The data used to build this dashboard is a parquet file that contains millions of AI detections. An AI was trained to detect urchins and lobsters from the laboratory videos collected by UTAS colleagues. For every frame of the video, the x and y coordinates of the object were exported.

## AI
The AI was trained using YoloV5 and FishID infrastructure. The AI was trained on a dataset of approximately 1,000 images and approximately 3,000 annotations for all classes. The model was evaluated with a test dataset of approximately 500 images and 1,500 annotations for all classes and achieved an F1 score of 0.97. The model's confidence was set to 0.2 to reduce the number of false positives.

## Data Wrangling
Significant data wrangling was performed to produce this dashboard. As a whole, the data wrangling process can be broken down into the following steps:
1. Import the data
2. Filter the data to only include frames where urchin(s) and lobster are present
3. Find the start and end frames when the urchin and lobster were present in the same frame
4. The start and end frames of potential interactions were named as Motifs
5. A consistent time sequence was created for each Motif of the event_ID

## Metrics
3 infoboxes were created to summarise the data:
1. the sea urchin maxn
2. the sea urchin median
3. An estimate if there was a predation event; based on if the count of predation motifs was >5 - Yes

2 graphs one showing the motif number and the start and end time of the motif and the other showing the number of frames per motif number. The final, and most important graph shows the detections of urchins and lobster as a sequence of events for those motifs >20 frames. 



