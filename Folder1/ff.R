# Install Essential Libraries for Plotting and the datasets that we are going to use. The datasets are comprised of
# various Flight Details for the years of 2003 & 2004 in USA.

install.packages("ggplot2")
library('ggplot2')
library('dplyr')
install.packages("gridExtra")
library('gridExtra')
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device = "win")


flights = read.csv("2003.csv.bz2") 
flights1 = read.csv('2004 (1).csv.bz2')


names(flights) # Access the Attributes of the flights
str(flights)# Get the DataType for all the attributes in the dataset



# Now, we aim to find the most common Flight routes between US Airports in 2003 & 2004 and the flight routes 
# that have been Cancelled a 'significant' number of times.


y = as.factor(flights$Origin)# We factorize the Origin Column, in order to be able to use the different Airport Names
z = levels(y)# Obtain the Origin Airport Names


which(is.na(flights$Dest) == TRUE)# No NA Values in Destination Airport
which(is.na(flights$Origin) == TRUE)# No NA Values Origin Airport

df_route = flights[which(row.names(flights) %in% sample(row.names(flights),size = 1000000,replace = FALSE)),
                     c('Dest','Origin','Cancelled')]# We Take a sample of the the Origin, Dest and Cancelled columns, since the volume of the dataset is too large 


# Now, we create to matrices that they are going to filled with the number of flights and Cancellations for each Route, respectively.  
routes = matrix(nrow = length(z), ncol = length(z))



n_routes = matrix(nrow = length(z), ncol = length(z))
n_routes_cancel = matrix(nrow = length(z), ncol = length(z))


# We fill out the matrices at hand with the following iteration.
for (i in 1:length(z)){
  for (j in 1:length(z)){
    if (z[i] != z[j]){
      n_routes[i,j] = length(df_route$Origin[(df_route$Dest == z[j]) & 
                                                     df_route$Origin == z[i]])
      }else{
        n_routes[i,j] = 0
    }
  }
}

for (i in 1:length(z)){
  for (j in 1:length(z)){
    if (z[i] != z[j]){
      n_routes_cancel[i,j] = length(df_route$Origin[(df_route$Dest == z[j]) & 
                                                 (df_route$Origin == z[i]) & (df_route$Cancelled == 1)])
    }else{
      n_routes_cancel[i,j] = 0
    }
  }
}

# Let us now create a barplot showing the most busy and the most cancelled flights in the along US Airports in 2003. In order to 
# to assess whether a  flight is popular or that a flight has been generally cancelled significant amount of times
# we set two threshold based on the descriptive statistics of the two variables.


busy_routes_names = paste(z[which(n_routes>=350, arr.ind = TRUE)[,1]]
                          ,z[which(n_routes>=350, arr.ind = TRUE)[,2]],sep = '-')
n_busy_flights = n_routes[which(n_routes>=350, arr.ind = TRUE)]

ggplot()+
  geom_col(aes(x = reorder(busy_routes_names, -n_busy_flights), n_busy_flights, fill = busy_routes_names))+
  labs(x = "Route", y = 'Number of flights', title = "Busiest Flight Routes in USA Airports in 2003")+
  theme(axis.title.x = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 16, margin = margin(t = 45, r = 20, b = 20, l = 0)),
        legend.title = element_blank())


lot_cancelled_flights = n_routes_cancel[which(n_routes_cancel>=10, arr.ind = TRUE)]
lot_cancelled_routes = paste(z[which(n_routes_cancel>=10, arr.ind = TRUE)[,1]]
                             ,z[which(n_routes_cancel>=10, arr.ind = TRUE)[,2]],sep = '-')
ggplot()+
  geom_col(aes(x = reorder(lot_cancelled_routes,- lot_cancelled_flights)
               , y = lot_cancelled_flights, fill = lot_cancelled_routes), width = 0.8)+
  labs(x = "Route", y = 'Number of Cancellations', title = "Most Cancelled Routes in US Airports in 2003")+
  theme(axis.title.x = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 16, margin = margin(t = 45, r = 20, b = 20, l = 0)),
        legend.title = element_blank())









#Lets check for NA Values in Flight Cancellations & Month



which(is.na(flights$Cancelled) == TRUE) # No NAN values in Cancelled Flights
which(is.na(flights$Month)==FALSE) # No Missing Month 

# Firstly, we are going to examine whether cancellations depend on the trip distance, on month or on the carrier as well.



cancel_per_month = aggregate(flights$Cancelled, by = flights['Month'], FUN = sum)# We find the cancellations per month

cancel_per_month = data.frame(cancel_per_month)# We convert into dataframe for easier manipulation

Months = as.factor(cancel_per_month[,1])# DO I NEED TO FACTORIZE IT?

Cancelations = cancel_per_month[,2]



ggplot(data = cancel_per_month)+
  geom_col(aes (Months, Cancelations))






# Now, we do the same but this time our classifying parameter will be the Carriers. We always start with a check for 
# NAN values.

summary(flights$UniqueCarrier) 

cancel_carrier = aggregate(flights$Cancelled, by = flights["UniqueCarrier"], FUN = sum)

Carrier = cancel_carrier[,1]

 

ggplot(cancel_carrier)+
  geom_col(aes(Carrier, cancel_carrier[,2]))







#Cancellations per Month in 2003&2004, Also the rates of cancellations among all flights per month the same years.
month_cancel = aggregate(flights$Cancelled, flights['Month'], FUN = 'sum')
month_cancel1 = aggregate(flights1$Cancelled, flights1['Month'], FUN = 'sum')


ggplot()+
  geom_line(data = month_cancel, aes(x = Month, y = x, color = '2003'), size = 1)+
  geom_point(data = month_cancel, aes(x = Month, y = x), size = 4)+
  geom_line(data = month_cancel1, aes(x = Month, y = x, color = '2004'), size = 1)+
  geom_point(data = month_cancel1, aes(x = Month, y = x), size = 4)+
  scale_x_continuous(breaks = seq(1,12,1))+
  labs(y = 'Number of Cancellations', title = "Number of Flight Cancellations per Month in USA Airports in 2003 & 2004",
       colour = 'Year')+
  theme(axis.title.x = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 14, margin = margin(t = 45, r = 20, b = 20, l = 0)))


total_flights_per_month = aggregate(flights$Month, flights['Month'], FUN = 'length')
total_flights_per_month1 = aggregate(flights1$Month, flights1['Month'], FUN = 'length')
rate = month_cancel$x/total_flights_per_month$x
rate1 = month_cancel1$x/total_flights_per_month1$x
rate_of_cancel_per_month = data.frame(total_flights_per_month$Month, rate)
rate_of_cancel_per_month1 = data.frame(total_flights_per_month1$Month, rate1)


ggplot()+
  geom_line(data = rate_of_cancel_per_month, aes(x = total_flights_per_month$Month, y = rate, color = '2003'), size = 1)+
  geom_point(data = rate_of_cancel_per_month, aes(x = total_flights_per_month.Month, y = rate), size = 4)+
  geom_line(data = rate_of_cancel_per_month1, aes(x = total_flights_per_month1.Month, y = rate1, color = '2004'), size = 1)+
  geom_point(data = rate_of_cancel_per_month1, aes(x = total_flights_per_month1.Month, y = rate1), size = 4)+
  scale_x_continuous(breaks = seq(1,12,1))+
  labs(y = 'Rate', title = "Percentage of Flight Cancellations per Month in USA Airports in 2003 & 2004",
       colour = 'Year', x = 'Month')+
  theme(axis.title.x = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 14, margin = margin(t = 45, r = 20, b = 20, l = 0)))





# Now, we try to find a pattern between the range of the flight and the prospect of being Cancelled.
#We construct a dataframe with respect to a  classification of the distance, that we decide. 
short = vector(mode = 'numeric',   length(flights$Cancelled))
medium = vector(mode = 'numeric', length(flights$Cancelled))
long = vector(mode = 'numeric', length(flights$Cancelled))
very_long = vector(mode = 'numeric', length(flights$Cancelled))


#The Classification proceeds with the following loop

for (i in (1:nrow(flights))) { 
  if (flights$Distance[i] <= summ['1st Qu.']){
      short[i] = 1
  } else if (summ['Mean'] >= flights$Distance[i] & flights$Distance[i] > summ['1st Qu.']){
      medium[i] = 1 
  } else if (summ['Mean'] < flights$Distance[i] & flights$Distance[i] <= summ['3rd Qu.']){
      long[i] = 1
  } else if (flights$Distance[i] > summ['3rd Qu.']){
      very_long[i] = 1
    
  }}


# The desired Dataframe
distances1 = data.frame('short' = short, 'Medium' = medium, 'long' = long , 'Very_long' = very_long)




#We can do the same but creating one character column and not four categorical

distances2 = vector(mode = 'character', length = nrow(flights))



for (i in (1:nrow(flights))) { 
  if (flights$Distance[i] <= summ['1st Qu.']){
      distances2[i] = 'short'
  } else if (summ['Mean'] >= flights$Distance[i] & flights$Distance[i] > summ['1st Qu.']){
      distances2[i] = 'medium' 
  } else if (summ['Mean'] < flights$Distance[i] & flights$Distance[i] <= summ['3rd Qu.']){
      distances2[i] = 'long'
  } else if (flights$Distance[i] > summ['3rd Qu.']){
      distances2[i] = 'very_long'
    
  }}  


# Lets insert this new attribute to our Dataframe. Moreover, we find the rate of cancelled flights 
# within each category of distance
flights['Distance2'] = distances2


cancel_distance = aggregate(flights$Cancelled, flights['Distance2'], FUN = 'sum')#Sum of cancelled flights based on the distance type


#Rates
cancel_very_long_rate = cancel_distance[1,2]/sum(very_long)
cancel_long_rate = cancel_distance[1,2]/sum(long)
cancel_medium_rate = cancel_distance[1,2]/sum(medium)
cancel_short_rate = cancel_distance[1,2]/sum(short)


# Let us first plot a bargraph showing the rate of cancellation within each distance class. Then, 
# we plot a standard bargraph counting based on distance type again.

x = c('Very_long', 'long', 'Medium', 'short')
y = c(cancel_very_long_rate,cancel_long_rate,cancel_medium_rate,cancel_short_rate)

ggplot()+
  geom_col(aes(x = reorder(x,-y), y, fill = y, width = 0.7))+
  theme(text = element_text(size = 15))+  
  labs(x = 'Distance', y = 'Rate')+
  labs(title = "Percentage of Cancelled flights wrt to the Flight Distance")+
  theme(axis.title.x = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
      axis.title.y = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
      plot.title = element_text(size = 18, margin = margin(t = 45, r = 20, b = 20, l = 0)))



ggplot(data = flights)+
  geom_bar(aes(x = reorder(distances2, distances2, function(x)-length(x)), fill = distances2))+
  labs(x ='Distance Type', y = "Number of flights")+
  theme(axis.title.x = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)))

# Hence , we see that is 3 times more likely for a long flight than a short one to be cancelled









# Let us now examine the Flight Delay times. We are going to check if there are any patterns
# between the month of departure and the delay. Firstly, we check for NAN Values in DepDelay and if exist
# we impute them using the sample mean.

summary_dep_delay = summary(flights$DepDelay)

na_dep_delay = which(is.na(flights$DepDelay) == TRUE)

for (i in na_dep_delay){
  flights$DepDelay[i] = summary_dep_delay['Mean']
}

summary(flights$DepDelay)# Check if there are any more NAN Values in DepDelay column.



# Average Departure Delay per month.
dep_delay_month = aggregate(flights$DepDelay, flights['Month'], FUN = 'sum')

dep_delay_month = data.frame(dep_delay_month)

for (i in 1:12){
  dep_delay_month[i,2] = dep_delay_month[i,2]/length(flights$DepDelay[flights$Month == i])
}# Obtain the desired average departure delays



ggplot(data = dep_delay_month)+
  geom_col(aes(x = Month, y = x))+
  scale_x_continuous(breaks = seq(1,12,1))+
  theme(axis.title.x = element_text(family ='mono', size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(family = 'mono', size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 13, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))+
  labs(y = 'Departure Delay', title = 'Average Departure Delay (in minutes) per Month')








# Let us now check the average departure delay on five different big airports in USA.


airport_dep_delay = flights$DepDelay[flights$Origin == "ATL" | flights$Origin == "JFK"
                                     |flights$Origin == "LAX"|flights$Origin == "DFW"
                                     |flights$Origin == "DEN"]# All Departure delays for airports under consideration




summary(airport_dep_delay)



airport_indx = which(flights$Origin == "ATL" | flights$Origin == "JFK"
                     |flights$Origin == "LAX"|flights$Origin == "DFW"
                     |flights$Origin == "DEN")# All the rows of 





airports_total_dep_delay = aggregate(airport_dep_delay, list(flights$Origin[airport_indx]),
                               FUN = 'sum')

len1 = length(flights$DepTime[flights$Origin == "ATL"])# In order to find the sample mean of departure delay in each airport
len2 = length(flights$DepTime[flights$Origin == "DEN"])
len3 = length(flights$DepTime[flights$Origin == "DFW"]) 
len4 = length(flights$DepTime[flights$Origin == "JFK"])
len5 = length(flights$DepTime[flights$Origin == "LAX"])

airports_total_dep_delay$x[1] = airports_total_dep_delay$x[1]/len1
airports_total_dep_delay$x[2] = airports_total_dep_delay$x[2]/len2
airports_total_dep_delay$x[3] = airports_total_dep_delay$x[3]/len3
airports_total_dep_delay$x[4] = airports_total_dep_delay$x[4]/len4
airports_total_dep_delay$x[5] = airports_total_dep_delay$x[5]/len5
  

ggplot(airports_total_dep_delay)+
  geom_col(aes(x = reorder(Group.1, x), y = x, fill = Group.1))+
  theme(axis.title.x = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 13, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))+
  labs(x = 'Airport' ,y = 'Departure Delay',
       title = 'Average Departure Delay (in minutes) in five Airports through 2003')











# Now, we move our focus to the ArrTime and DepTime of the flights. We are going to check
# the busiest hours in the Atlanta+JFK+LAX Airport for the whole year. We start by creating 
# a new column with the departure time converted into date format.



flights$dep_time = sprintf("%04d", flights$DepTime)
flights$dep_time = format(strptime(flights$dep_time, format="%H%M"), format = "%H:%M")



which(is.na(flights$dep_time)==TRUE)# There are NAN Values


flights_per_minute_atlanta = aggregate(flights$dep_time[flights$Origin == "ATL"], 
                                       list(flights$dep_time[flights$Origin == 'ATL']), FUN = 'length')

flights_per_minute_queens = aggregate(flights$dep_time[flights$Origin == "JFK"], 
                                                  list(flights$dep_time[flights$Origin == 'JFK']), FUN = 'length')

flights_per_minute_los_ang = aggregate(flights$dep_time[flights$Origin == "LAX"], 
                                                  list(flights$dep_time[flights$Origin == 'LAX']), FUN = 'length')


flights_per_minute_atlanta$Group.1 = as.POSIXct(flights_per_minute_atlanta$Group.1, format = "%H:%M")
flights_per_minute_queens$Group.1 = as.POSIXct(flights_per_minute_queens$Group.1, format = "%H:%M")
flights_per_minute_los_ang$Group.1 = as.POSIXct(flights_per_minute_los_ang$Group.1, format = "%H:%M")


ggplot()+
  geom_line(data = flights_per_minute_atlanta, aes(x = Group.1, y = x, color = 'ATL'))+
  theme(axis.title.x = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 13, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))+
  labs(x = 'Time', y = 'Number of flights', title = 'Number of flight deparutes through an average day in ATL, JFK & LAX Airport (2003)')+
  geom_line(data = flights_per_minute_queens, aes(x = Group.1, y = x, color = 'JFK'))+
  geom_line(data = flights_per_minute_los_ang, aes(x = Group.1, y = x, color = 'LAX'))
 

# Lets now do the same but for all the airports in USA in 2003 and 2004
flights1$dep_time = sprintf("%04d", flights1$DepTime)
flights1$dep_time = format(strptime(flights1$dep_time, format="%H%M"), format = "%H:%M")

flights_per_min = aggregate(flights$dep_time, flights['dep_time'], FUN = 'length')
flights1_per_min = aggregate(flights1$dep_time, flights1['dep_time'], FUN = 'length')

flights_per_min$dep_time = as.POSIXct(flights_per_min$dep_time, format = "%H:%M")
flights1_per_min$dep_time = as.POSIXct(flights1_per_min$dep_time, format = "%H:%M")


first = 
  ggplot()+
  theme(axis.title.x = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 10, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))+
  labs(x = 'Time', y = 'Number of flights', title = 'Number of flight deparutes through a day in US Airports in 2003')+
  geom_line(data = flights_per_min, aes(x = dep_time, y = x, color = '2003'), size = 0.1, color = 'black')
 


second =
  ggplot()+
  theme(axis.title.x = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 10, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))+
  labs(x = 'Time', y = 'Number of flights', title = 'Number of flight deparutes through a day in US Airports in 2004')+
  geom_line(data = flights1_per_min, aes(x = dep_time, y = x, color = '2004'), size = 0.1, colour = 'blue')
  



grid.arrange(first, second, ncol = 2, nrow = 1)# No Difference between 2003 & 2004 in the amount of flights per minute on an average day




#Now, lets separate the Flight Departures per minute on a Summer and on a Winter day.

dep_time_winter = flights1$dep_time[flights$Month == c(11,12,1,2)]
dep_time_summer = flights1$dep_time[flights$Month == c(5,6,7,8)]


flights1_per_min_winter = aggregate(dep_time_winter, list(dep_time_winter), "length")
flights1_per_min_summer = aggregate(dep_time_summer, list(dep_time_summer), "length")

flights1_per_min_winter$Group.1 = as.POSIXct(flights1_per_min_winter$Group.1, format = "%H:%M")
flights1_per_min_summer$Group.1 = as.POSIXct(flights1_per_min_summer$Group.1, format = "%H:%M")

winter = 
  ggplot()+
    geom_line(data = flights1_per_min_winter, aes(x = Group.1, y = x, color = "Winter"))
    
summer = 
  ggplot()+
    geom_line(data = flights1_per_min_summer, aes(x = Group.1, y = x, color = "Summer"))

grid.arrange(winter, summer, ncol = 2, nrow = 1)# Again, no difference in the number of Flights per minute through a Summer day or on a Winter day



# The same for the Arrival time

flights$arr_time = sprintf("%04d", flights$ArrTime)
flights$arr_time = format(strptime(flights$arr_time, format="%H%M"), format = "%H:%M")


flights1$arr_time = sprintf("%04d", flights1$ArrTime)
flights1$arr_time = format(strptime(flights1$arr_time, format="%H%M"), format = "%H:%M")

flights_arr_per_min = aggregate(flights$arr_time, flights['arr_time'], FUN = 'length')
flights1_arr_per_min = aggregate(flights1$arr_time, flights1['arr_time'], FUN = 'length')



flights_arr_per_min$arr_time = as.POSIXct(flights_arr_per_min$arr_time, format = "%H:%M")
flights1_arr_per_min$arr_time = as.POSIXct(flights1_arr_per_min$arr_time, format = "%H:%M")


first_arr = 
  ggplot()+
  theme(axis.title.x = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 10, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))+
  labs(x = 'Time', y = 'Number of flights', title = 'Number of flight arrivals through a day in US Airports in 2003')+
  geom_line(data = flights_arr_per_min, aes(x = arr_time, y = x, color = '2003'), size = 0.1, color = 'black')



second_arr =
  ggplot()+
  theme(axis.title.x = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 13, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 10, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))+
  labs(x = 'Time', y = 'Number of flights', title = 'Number of flight arrivals through a day in US Airports in 2004')+
  geom_line(data = flights1_arr_per_min, aes(x = arr_time, y = x, color = '2004'), size = 0.1, colour = 'blue')



grid.arrange(first_arr, second_arr, ncol = 2, nrow = 1)


## Reason of (Arrival and Departure) Delay in ATL Airport###########################


samp_w_delay = sample(x = flights$WeatherDelay[flights$Origin == 'ATL'], size = 10000)# We take samples
samp_c_delay = sample(flights$CarrierDelay[flights$Origin == 'ATL'], size = 10000)
samp_secur_delay = sample(flights$SecurityDelay[flights$Origin == 'ATL'], size = 10000)
samp_NAS_delay = sample(flights$NASDelay[flights$Origin == 'ATL'], size = 10000)
samp_late_aircraft_delay = sample(flights$LateAircraftDelay[flights$Origin == 'ATL'], size = 10000)


 
na_indx_secure = which(is.na(samp_secur_delay) == TRUE)# We Impute the NAN Values with the mean
na_indx_c = which(is.na(samp_c_delay) == TRUE)
na_indx_w = which(is.na(samp_w_delay) == TRUE)
na_indx_NAS = which(is.na(samp_NAS_delay) == TRUE)
na_indx_late_aircraft = which(is.na(samp_late_aircraft_delay) == TRUE)

for (i in na_indx_secure){
  samp_secur_delay[i] = summary(samp_secur_delay)['Mean']
}

for (i in na_indx_c){
  samp_c_delay[i] = summary(samp_c_delay)['Mean']
}

for (i in na_indx_w){
  samp_w_delay[i] = summary(samp_w_delay)['Mean']
}

for (i in na_indx_NAS){
  samp_NAS_delay[i] = summary(samp_NAS_delay)['Mean']
}

for (i in na_indx_late_aircraft){
  samp_late_aircraft_delay[i] = summary(samp_late_aircraft_delay)['Mean']
}



delays = data.frame(samp_c_delay,samp_secur_delay,samp_w_delay,samp_NAS_delay,samp_late_aircraft_delay)

delays$Type = vector(mode = "character", length = nrow(delays))

str(flights)

# Now, as the dep_delay is numeric measured in minutes we need to decide whether there is a 'reasonable' delay due to each one of these reasons.
# We set our threshold to 10 minutes.

for (i in 1:nrow(delays)){
  if (delays$samp_c_delay[i] > 10){
    delays$Type[i] = "Carrier"
  }else if (delays$samp_secur_delay[i] > 10){
    delays$Type[i] = "Security"
  }else if (delays$samp_w_delay[i] > 10){
    delays$Type[i] = "Weather"
  }else if (delays$samp_NAS_delay[i] > 10){
    delays$Type[i] = "NAS"
  }else if (delays$samp_late_aircraft_delay[i] > 10){
    delays$Type[i] = "Late Aircraft"
  }
}


empty_indx = which(delays$Type == "")

delays = delays[-empty_indx,]


ggplot(delays, aes(x = Type, fill = Type))+
  geom_bar(aes(x = reorder(Type, Type, function(x)-length(x)),y = (..count..)/sum(..count..)))+
  labs(x = "Type of Delay", y = "Percentage", title = "Percentage of delayed flights based on Delay Type in ATL Airport ")+
  theme(axis.title.x = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 15, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))






#The same as previously, but for all Airports in USA 

samp_w_delay_t = sample(x = flights$WeatherDelay, size = 10000)# We take samples
samp_c_delay_t = sample(flights$CarrierDelay, size = 10000)
samp_secur_delay_t = sample(flights$SecurityDelay, size = 10000)
samp_NAS_delay_t = sample(flights$NASDelay, size = 10000)
samp_late_aircraft_delay_t = sample(flights$LateAircraftDelay, size = 10000)



na_indx_secure_t = which(is.na(samp_secur_delay_t) == TRUE)# We Impute the NAN Values with the mean
na_indx_c_t = which(is.na(samp_c_delay_t) == TRUE)
na_indx_w_t = which(is.na(samp_w_delay_t) == TRUE)
na_indx_NAS_t = which(is.na(samp_NAS_delay_t) == TRUE)
na_indx_late_aircraft_t = which(is.na(samp_late_aircraft_delay_t) == TRUE)

for (i in na_indx_secure_t){
  samp_secur_delay_t[i] = summary(samp_secur_delay_t)['Mean']
}

for (i in na_indx_c_t){
  samp_c_delay_t[i] = summary(samp_c_delay_t)['Mean']
}

for (i in na_indx_w_t){
  samp_w_delay_t[i] = summary(samp_w_delay_t)['Mean']
}

for (i in na_indx_NAS_t){
  samp_NAS_delay_t[i] = summary(samp_NAS_delay_t)['Mean']
}

for (i in na_indx_late_aircraft_t){
  samp_late_aircraft_delay_t[i] = summary(samp_late_aircraft_delay_t)['Mean']
}

summary(samp_NAS_delay_t)

delays_t = data.frame(samp_c_delay_t,samp_secur_delay_t,samp_w_delay_t,samp_NAS_delay_t,samp_late_aircraft_delay_t)

delays_t$Type = vector(mode = "character", length = nrow(delays_t))


# Now, we need to decide whether there is reasonable delay due to each one of these reasons

for (i in 1:nrow(delays_t)){
  if (delays_t$samp_c_delay_t[i] > 10){
    delays_t$Type[i] = "Carrier"
  }else if (delays_t$samp_secur_delay_t[i] > 10){
    delays_t$Type[i] = "Security"
  }else if (delays_t$samp_w_delay_t[i] > 10){
    delays_t$Type[i] = "Weather"
  }else if (delays_t$samp_NAS_delay_t[i] > 10){
    delays_t$Type[i] = "NAS"
  }else if (delays_t$samp_late_aircraft_delay_t[i] > 10){
    delays_t$Type[i] = "Late Aircraft"
  }
}


empty_indx_t = which(delays_t$Type == "")

delays_t = delays_t[-empty_indx_t,]


ggplot(delays_t, aes(x = Type, fill = Type))+
  geom_bar(aes(x = reorder(Type, Type, function(x)-length(x)),y = (..count..)/sum(..count..)))+
  labs(x = "Type of Delay", y = "Percentage", title = "Percentage of delayed flights based on Delay Type in USA Airports in 2003 ")+
  theme(axis.title.x = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.title.y = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 15, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))

############################################################---############################################################



# Let us now find the best day on a week for avoiding delays in US Airports in 2003 & 2004

#We start by taking a sample of Departure Delays and the relevant Month and Days of the Week 

sample_dep_delay_day = flights[which(row.names(flights) %in% sample(row.names(flights), size = 1000000,
                                                                    replace = FALSE)), 
                               c('DepDelay','DayOfWeek',"Month")]

sample_dep_delay_day_1 = flights1[which(row.names(flights1) %in% sample(row.names(flights1), size = 1000000,
                                                                    replace = FALSE)), 
                               c('DepDelay','DayOfWeek',"Month")]

summary(sample_dep_delay_day_1)# There are NAN Values in our sample and we are going to impute them as follows

summary(flights1$DayOfWeek)

for (i in which(is.na(sample_dep_delay_day$DepDelay) == TRUE)){
  sample_dep_delay_day$DepDelay[i] = summary(sample_dep_delay_day$DepDelay)['Mean']
}

for (i in which(is.na(sample_dep_delay_day_1$DepDelay) == TRUE)){
  sample_dep_delay_day_1$DepDelay[i] = summary(sample_dep_delay_day_1$DepDelay)['Mean']
}

days_delay = aggregate(sample_dep_delay_day$DepDelay, sample_dep_delay_day['DayOfWeek'], FUN = 'mean')
days_delay_1 = aggregate(sample_dep_delay_day_1$DepDelay, sample_dep_delay_day_1['DayOfWeek'], FUN = 'mean')

first_year =
  ggplot(days_delay)+
    geom_col(aes(DayOfWeek, x), fill = 'darkorange4')+
    scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
    labs(x = "Day of the Week", y = "Minutes", title = "Average delay in flights per day in US Airports in 2003 ")+
    theme(axis.title.x = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
          axis.title.y = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
          plot.title = element_text(size = 15, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))

second_year = 
  ggplot(days_delay_1)+
    geom_col(aes(DayOfWeek, x), fill = 'firebrick4')+
    scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
    labs(x = "Day of the Week", y = "", title = "Average delay in flights per day in US Airports in 2004 ")+
    theme(axis.title.x = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
          axis.title.y = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
          plot.title = element_text(size = 15, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)))



grid.arrange(first_year, second_year, ncol = 2, nrow = 1)

# Moreover, we separate the year in Winter and Summer Period, and we are looking for any 'significant'
# difference in the average Departure Delay between Summer and Winter days of the week.

for (i in 1:nrow(sample_dep_delay_day)){
  if ((1<=sample_dep_delay_day$Month[i] && sample_dep_delay_day$Month[i]<=4) | 
      (9 <= sample_dep_delay_day$Month[i] && sample_dep_delay_day$Month[i] <= 12)){
    sample_dep_delay_day$Season[i] = 'Winter'
  }else{
    sample_dep_delay_day$Season[i] = "Summer"
  }
}


df = aggregate(.~Season+DayOfWeek, sample_dep_delay_day, mean)# The average departure delay per day of the week and per Period(Summer/Winter)


ggplot(df)+
  geom_col(aes(x = DayOfWeek, y = DepDelay, fill = Season), position = 'dodge', stat = 'identity')+
  scale_x_discrete(limits = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
                 
 


#Now, let us examine if there is any difference in the Departure Delay of the flights in all US 
#Airports between the Weekdays and the Weekends. To do so, we first separate the DepDelay attribute
#into Weekdays and Weekends and then we find the average of DepDelay, as the total number would not
# be representative since Weekdays are more than the Weekends.














#Rate Cancelations Difference between 2003 & 2004 in various big Airports.
install.packages('dplyr')
library(dplyr)

airports = as.factor(flights$Origin)


airport_cancel_rate_diff = aggregate(flights$Origin, flights["Origin"], "length")
l = which(airport_cancel_rate_diff$x >100000)
airport_cancel_rate_diff = airport_cancel_rate_diff[l,]

for (i in 1:nrow(airport_cancel_rate_diff)){
  airport_cancel_rate_diff$cancel_2003[i] = sum(flights$Cancelled[flights$Origin == airport_cancel_rate_diff$Origin[i]])
  airport_cancel_rate_diff$cancel_2004[i] = sum(flights1$Cancelled[flights1$Origin == airport_cancel_rate_diff$Origin[i]])
  airport_cancel_rate_diff$x1[i] = length(flights1$Cancelled[flights1$Origin ==airport_cancel_rate_diff$Origin[i]])
}



airport_cancel_rate_diff$rate = airport_cancel_rate_diff$cancel_2003 / airport_cancel_rate_diff$x 
airport_cancel_rate_diff$rate_1 = airport_cancel_rate_diff$cancel_2004 / airport_cancel_rate_diff$x1 
airport_cancel_rate_diff$rate_diff = airport_cancel_rate_diff$rate_1 - airport_cancel_rate_diff$rate
airport_cancel_rate_diff$rate_diff = 100*airport_cancel_rate_diff$rate_diff



ggplot(airport_cancel_rate_diff)+
  geom_col(aes(x = Origin, y = rate_diff), fill = 'mediumvioletred', width = 0.5)+
  
  labs(x = "", y = "", title = " Flight Cancelation Rate Difference in various Airports between 2003 & 2004 (%) ")+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 15, margin = margin(t = 15, r = 20, b = 0, l = 0)),
        axis.text.y = element_text(face = 'bold', size = 8),
        plot.title = element_text(size = 15, face = 'bold', margin = margin(t = 45, r = 20, b = 20, l = 0)),
        panel.background = element_blank(),
        axis.line.y.left = element_line(linetype = 'solid',size = 0.5)
        )+
  coord_flip()+
  geom_hline(yintercept = seq(-1,1,0.5), linetype = 'dotted')



       