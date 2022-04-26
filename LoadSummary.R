temp = read.csv("Summary.csv")

TestsNew = as.data.frame(temp) %>%
    arrange(County)%>%
    mutate(
        # Active = Total.Positive.Tests 
        #    - Total.Recovered
        #    - Total.Deaths,
           Date = as.Date(date))

TestsOld = read.csv("TestsOld.csv")
TestsOld = TestsOld %>%
    mutate(Date = as.Date(Date))%>%
    select(County,Total.Positive.Tests,
           Total.Positive.Cases,
           Total.Deaths,
           Date)
temp1 = TestsNew %>%
    select(County,Total.Positive.Tests, 
           Total.Positive.Cases,
           Total.Deaths,
           Date)

temp = temp1[,-1]-TestsOld[,-1] 
# temp = cbind(County = TestsNew[,1], temp)
# temp = temp %>%
#     mutate(
#         # Percent.Daily.Tests = Total.Tests/as.numeric(Date),
#         Percent.Daily.Positive = Total.Positive.Tests/as.numeric(Date),
#         # Percent.Daily.Recovered = Total.Recovered/as.numeric(Date)
#         )

# New.Tests = read.csv("NewTests.csv")
New.Positive.Tests = read.csv("NewPositiveTests.csv")
New.Positive.Cases = read.csv("NewPositiveCases.csv")

# New.Recovered = read.csv("NewRecovered.csv")
New.Deaths = read.csv("NewDeaths.csv")
# d_temp = as.data.frame(t(TestsOld))
d_temp = as.data.frame(t(temp[,-4]))
# names(d_temp) = temp[,1]
names(d_temp) = names(New.Positive.Tests)[-1]
d_temp[1:3,] = sapply(d_temp[1:3,], as.integer)
d_temp = cbind(Date = date, d_temp)
# New.Tests = rbind(New.Tests,d_temp["Total.Tests",]) %>%
#     unique()
New.Positive.Tests = rbind(New.Positive.Tests, d_temp["Total.Positive.Tests",])%>%
    unique()
# New.Recovered = rbind(New.Recovered, d_temp["Total.Recovered",])%>%
    # unique()
New.Deaths =rbind(New.Deaths, d_temp["Total.Deaths",])%>%
    unique()
New.Positive.Cases = rbind(New.Positive.Cases, 
                           d_temp["Total.Positive.Cases",])%>%
    unique()

# write.csv(New.Tests, "NewTests.csv",row.names = FALSE)
write.csv(New.Positive.Tests, "NewPositiveTests.csv",row.names = FALSE)
write.csv(New.Positive.Cases, "NewPositiveCases.csv",row.names = FALSE)

# write.csv(New.Recovered, "NewRecovered.csv",row.names = FALSE)
write.csv(New.Deaths, "NewDeaths.csv",row.names = FALSE)

TestsOld = TestsNew
write.csv(TestsOld,"TestsOld.csv", row.names = FALSE)
