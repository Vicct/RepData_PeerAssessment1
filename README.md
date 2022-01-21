<h1 class="title toc-ignore">Activity_Steps</h1>
<h4 class="author">Victor Campos</h4>
<h4 class="date">20/1/2022</h4>

</div>


<p>It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.</p>
<p>This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.</p>
<p>The data for this assignment can be downloaded from the course web site:</p>
<p>Dataset: Activity monitoring data [52K]</p>
<p>The variables included in this dataset are:</p>
<p>steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)</p>
<p>date: The date on which the measurement was taken in YYYY-MM-DD format</p>
<p>interval: Identifier for the 5-minute interval in which measurement was taken</p>
<p>The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.</p>
<p>Full submission</p>
<ol style="list-style-type: decimal">
<li>Code for reading in the data set and/or processing the data</li>
</ol>
<pre class="r"><code>library(knitr)</code></pre>
<pre><code>## Warning: package &#39;knitr&#39; was built under R version 4.1.1</code></pre>
<pre class="r"><code>setwd(&quot;C:/Project Files/CURSOS/DataScience-Hopkins/ReproducibleResearch_Week 2_Course Project_1&quot;)</code></pre>
<pre class="r"><code>activitydata &lt;- read.csv(&quot;C:/Project Files/CURSOS/DataScience-Hopkins/ReproducibleResearch_Week 2_Course Project_1/activity.csv&quot;)
StepsPerDay &lt;- aggregate(steps~date, activitydata, sum)
hist(StepsPerDay$steps, col = &quot;darkblue&quot;, xlab = &quot;Total steps per day&quot;, ylim = c(0,30), main = &quot;Total number of steps taken each day&quot;, breaks = seq(0,25000,by=2500))</code></pre>

![image](https://user-images.githubusercontent.com/25466408/150454508-75a90ec2-9e3a-4a06-84a1-575b81653341.png)

<pre class="r"><code>summary(activitydata)</code></pre>
<pre><code>##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA&#39;s   :2304</code></pre>
<ol start="2" style="list-style-type: decimal">
<li>Histogram of the total number of steps taken each day</li>
</ol>
<pre class="r"><code>library(ggplot2)
library(scales)
ggplot(StepsPerDay, aes(x=date, y=steps)) + geom_bar(stat = &quot;identity&quot;, width=.6, fill=&quot;blue&quot;) + ylab(&quot;Steps&quot;) + xlab(&quot;Dates&quot;) + labs(title = &quot;Total number of steps taken each day&quot;) + theme(axis.text.x = element_text(angle=75, vjust=0.1))</code></pre>

![image](https://user-images.githubusercontent.com/25466408/150454664-62be5d32-01ae-422c-ad92-7a62bbfe8fb8.png)

<li>Mean and median number of steps taken each day</li>
</ol>
<pre class="r"><code>rmean &lt;- mean(StepsPerDay$steps)
rmean</code></pre>
<pre><code>## [1] 10766.19</code></pre>
<pre class="r"><code>rmedian &lt;- median(StepsPerDay$steps)
rmedian</code></pre>
<pre><code>## [1] 10765</code></pre>
<ol start="4" style="list-style-type: decimal">
<li>Time series plot of the average number of steps taken</li>
</ol>
<pre class="r"><code>AverageStepsPerDay &lt;- aggregate(steps~date, activitydata, mean , na.rm=TRUE)
names(AverageStepsPerDay)&lt;-c(&quot;interval&quot;, &quot;mean&quot;)
ggplot(AverageStepsPerDay, aes(x=interval, y = mean , group=1)) + geom_line() + ylab(&quot;Average number of steps&quot;) + xlab(&quot;Interval&quot;) + labs(title = &quot;Average number of steps per interval&quot;) + theme(axis.text.x = element_text(angle=75, vjust=0.1))</code></pre>

![image](https://user-images.githubusercontent.com/25466408/150454738-6d6aeb4b-1ade-42f8-bc9f-3f0f7a8850e6.png)

<li>The 5-minute interval that, on average, contains the maximum number of steps</li>
</ol>
<pre class="r"><code>max_interval &lt;- activitydata[which.max(activitydata$steps),]$interval
max_interval</code></pre>
<pre><code>## [1] 615</code></pre>
<ol start="6" style="list-style-type: decimal">
<li>Code to describe and show a strategy for imputing missing data</li>
</ol>
<pre class="r"><code>TotalNAs &lt;- sum(!complete.cases(activitydata))
TotalNAs</code></pre>
<pre><code>## [1] 2304</code></pre>
<ol start="7" style="list-style-type: decimal">
<li>Histogram of the total number of steps taken each day after missing values are imputed</li>
</ol>
<pre class="r"><code>StepsAverage &lt;- aggregate(steps ~ interval, data = activitydata, FUN = mean)
fillNA &lt;- numeric()
for (i in 1:nrow(activitydata)) {
  obs &lt;- activitydata[i, ]
  if (is.na(obs$steps)) {
    steps &lt;- subset(StepsAverage, interval == obs$interval)$steps
  } else {
    steps &lt;- obs$steps
  }
  fillNA &lt;- c(fillNA, steps)
}

new_dataset_activity &lt;- activitydata
new_dataset_activity$steps &lt;- fillNA

TotalSteps &lt;- aggregate(steps ~ date, data = new_dataset_activity, sum, na.rm = TRUE)
hist(TotalSteps$steps, main = paste(&quot;Total Steps Each Day&quot;), col=&quot;blue&quot;, xlab=&quot;Number of Steps&quot;)</code></pre>

![image](https://user-images.githubusercontent.com/25466408/150454814-569eb031-2171-460c-abf4-0a22cbd55dab.png)

<p>7.1 Create Histogram to show difference.</p>
<pre class="r"><code>hist(TotalSteps$steps, main = paste(&quot;Total Steps Each Day&quot;), col=&quot;blue&quot;, xlab=&quot;Number of Steps&quot;)
hist(StepsPerDay$steps, main = paste(&quot;Total Steps Each Day&quot;), col=&quot;green&quot;, xlab=&quot;Number of Steps&quot;, add=T)
legend(&quot;topright&quot;, c(&quot;Imputed&quot;, &quot;Non-imputed&quot;), col=c(&quot;blue&quot;, &quot;green&quot;), lwd=10)</code></pre>

![image](https://user-images.githubusercontent.com/25466408/150454851-3a20e108-7074-4dc1-a78f-ebdd25e65f17.png)

<p>7.2 Mean and Median of the total number of steps taken per day</p>
<pre class="r"><code>cmean &lt;- mean(TotalSteps$steps)
cmean</code></pre>
<pre><code>## [1] 10766.19</code></pre>
<pre class="r"><code>cmedian &lt;- median(TotalSteps$steps)
cmedian</code></pre>
<pre><code>## [1] 10766.19</code></pre>
<ol start="8" style="list-style-type: decimal">
<li>Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends</li>
</ol>
<pre class="r"><code>weekdays &lt;- c(&quot;lunes&quot;, &quot;martes&quot;, &quot;miercoles&quot;, &quot;jueves&quot;, &quot;viernes&quot;)

new_dataset_activity$dayoftheweek = as.factor(ifelse(is.element(weekdays(as.Date(new_dataset_activity$date)),weekdays), &quot;Weekday&quot;, &quot;Weekend&quot;))
TotalStepsU &lt;- aggregate(steps ~ interval + dayoftheweek, new_dataset_activity, mean)

library(lattice)
xyplot(TotalStepsU$steps ~ TotalStepsU$interval|TotalStepsU$dayoftheweek, main=&quot;Average Steps per Day by Interval&quot;,xlab=&quot;Interval&quot;, ylab=&quot;Steps&quot;,layout=c(1,2), type=&quot;l&quot;)</code></pre>
</div>

![image](https://user-images.githubusercontent.com/25466408/150454884-2708d40b-6819-4203-8e4f-202682d8b6e8.png)

