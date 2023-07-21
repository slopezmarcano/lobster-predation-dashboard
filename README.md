# Dashboard to review Lobster-Urchin predation events

## Description
This dashboard is designed to allow users to review the predation events that have been identified by the Lobster-Urchin project. The dashboard is built using the R package `shiny` and is hosted on the [shinyapps.io](https://www.shinyapps.io/) platform. The dashboard can be accessed at the following link: https://lobster-urchin.shinyapps.io/predation_dashboard/

## Data
The data used to build this dashboard is a parquet file that contains millions of AI detections. An AI was trained to detect urchins and lobsters from the laboratory videos collected by UTAS colleagues. For every frame of the video, the x and y coordinates of the object were exported.

## AI
The AI was trained using YoloV5 using FishID infrastructure. The AI was trained on a dataset of approximately 1000 images and approximately 3,000 annotations for all classess. The model was evaluated with a test dataset of approximately 500 images and 1,500 annotations for all classess and achieved an F1 score of 0.97. The model's confidence was set to 0.2 to reduce the number of false positives.

## Data Wrangling
Significant data wrangling was performed to produce this dashboard. As a whole, the data wrangling process can be broken down into the following steps:
1. Import the data
2. Filter the data to only include frames where urchin(s) and lobster are present
3. Find the start and end frames when the urchin and lobster were present in the same frame
4. The start and end frames of potential interactions were named as Motifs
5. A consistent time sequence was created for each Motif of the event_ID


