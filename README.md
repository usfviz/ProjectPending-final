# Baseball Statistics 

## Joshua Amunrud - jamunrud@usfca.edu
## Danny Suh - hsuh2@usfca.edu

### Goal of the project
Goal of the project is to show major league baseball data from different angles. Specifically, we focused on batting statistics, such as batting average, locations of the in-play balls, etc.

### Data used
We used batting data of Los Angeles Dodgers batters in 2015 season. We fetched the data from pitchFx and MLBAM GameDay XML files. The data contains 12057 row, each row being a single event. The event data contains information such as batter id, name of the batter, result of the play, type of pitch the batter took, date/time of the event, and so on.

### Interface
![Alt text](/screenshot/Screen%20Shot%202017-05-01%20at%205.57.42%20PM.png)
![Alt text](/screenshot/Screen%20Shot%202017-05-01%20at%205.57.57%20PM.png)
![Alt text](/screenshot/Screen%20Shot%202017-05-01%20at%205.58.10%20PM.png)
![Alt text](/screenshot/Screen%20Shot%202017-05-01%20at%205.58.17%20PM.png)

In our interface, we have a drop down menu for selecting which batter to look at. Right below, we have tabs for seleceting different plots. For now we have spray plot of the batted balls, and timeseries plot of statistics. In the future, we plan to add heatmap of batting average in the strike zone, and parallel coordinate plot of types of pitches.

The spray plot shows where the in-play balls were located on the field. Each point is color coded with the result of the played ball. When user hovers over the point, tooltip shows the description of the play in detail.

The timeseries plot shows the trend of batter's statistics by month. We show batting average, on-base percentage and slugging percentage. User can hover over data points to see detailed stats (total plate appearance, total at-bats, total hits).
