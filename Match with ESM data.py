#!/usr/bin/env python
# coding: utf-8

# ## ESM Data

# In[ ]:


# Set the BEHAPP environment
get_ipython().run_line_magic('env', 'BEHAPP_ENV=prod')
get_ipython().run_line_magic('env', 'BEHAPP_ON_CLOUD=true')
get_ipython().run_line_magic('env', 'GOOGLE_CLOUD_PROJECT=true')

from behapp import Participant
from behapp import Study
import pandas as pd
import datetime
from datetime import date, timedelta
import numpy as np
import plotly.graph_objects as go
import plotly.express as px
import app_cleaning as ap
import time
import numpy

pd.set_option('display.max_columns', 10)
pd.set_option('display.max_rows', 10)


# In[ ]:


######## Define Function to calculate Features ##############
#############################################################

def ESM_passivemeasures(participant,rule = '3h', rule_int = 3, app_cleaning = True, Missing_Time = 24):
    
    participant = Participant.get(int(participant))

    ######## Load Data and Prepare them ##############
    ##################################################

    # Load ESM data
    ESM = pd.read_csv('/Users/annalangener/Nextcloud/BEHAPP data/SocialContext.csv', sep=',')
    ESM = ESM[ESM["questionListName"] != "Backlog Interaction"]
    ESM_time = ESM[ESM.loc[:,'BEHAPP_ID'] == participant.id]
    ESM_time.loc[:,'index_time'] = pd.to_datetime(ESM_time.loc[:,'index_time'])
    ESM_time = ESM_time.reset_index()
    ESM_time['Date'] = ESM_time["index_time"]

    # Info:
    # For intervall data we create a dataframe that shows everything per second. We can use this dataframe later to calculate the differen features
    # Code that was used to create dataframes per second (df1,df2,df3) is adapted from here: 
    # https://stackoverflow.com/questions/56341590/unstacking-shift-data-start-and-end-time-into-hourly-data.

    ### Calls

    calld = participant.data.call

    if len(calld)>0:
        #add end time to calls
        end_time = []
        for i in range(0,calld.shape[0]):
            durationN = calld["duration"][i]
            start_time = calld.index[i]
            end_time2 = calld.index[i] + timedelta(seconds=int(durationN))
            end_time.append(end_time2)
        calld["end_time"] = end_time

        calld = calld.reset_index()
        calld['recorded_naive'] = pd.to_datetime(calld['recorded_naive'])

        # Create a dataset that shows whether someone was calling or not per second

        df3 = pd.DataFrame([(z, w) for x, y, w in zip(calld['recorded_naive'], 
                                                      calld['end_time'] - pd.Timedelta(1, unit='s'), 
                                                      calld['type']) for z in pd.date_range(x, y, freq='s')], 
                           columns=['Date','type']) 

        # Then we do the same for the type pf call
        df3b = pd.DataFrame([(z, w) for x, y, w in zip(calld['recorded_naive'], 
                                                              calld['end_time'] - pd.Timedelta(1, unit='s'), 
                                                              calld['caller_hash']) for z in pd.date_range(x, y, freq='s')], 
                                   columns=['Date','caller_hash']) 

        # Merge both dataframes
        df3 = pd.merge(df3, df3b, how = 'left')

    ### Location
    staypoints = participant.views.staypoints # Staypoint dataset
    locraw = participant.data.location # Raw GPS points, used for data labeling
    locraw['index_time'] = locraw.index 

    if len(staypoints) > 0:

        #### Rename clusters based on frequency of visits

        # Sort staypoints by frequency of visit (!= total minutes spent!!)
        staypoints_sort = staypoints.groupby(['cluster_label']).count()
        staypoints_sort = staypoints_sort.reset_index()
        staypoints_sort = staypoints_sort.sort_values("time_spent_minutes",ascending=False)

        # Add new name
        staypoints_sort["cluster_label_new"] = 0
        staypoints_sort.loc[staypoints_sort["cluster_label"] != "HOME", "cluster_label_new"] = list(range(1,staypoints_sort.shape[0]))
        staypoints_sort.loc[staypoints_sort["cluster_label"] == "HOME", "cluster_label_new"] = "HOME"

        staypoints_sort = staypoints_sort[["cluster_label","cluster_label_new"]]

        # Merge with staypoint data set
        staypoints = pd.merge(staypoints,staypoints_sort, how = "left")

        # Create a dataset that shows on which staypoint someone was per second

        df2 = pd.DataFrame([(z, w) for x, y, w in zip(staypoints['arrival_time'], 
                                                              staypoints['leave_time'] - pd.Timedelta(1, unit='s'), 
                                                              staypoints['cluster_label_new']) for z in pd.date_range(x, y, freq='s')], 
                                   columns=['Date','Cluster']) 



    ### App
    # app_cleaning will avoid overlapping app usage.
    if app_cleaning == True:
        app = ap.app_cleaning(participant.id)
    if app_cleaning == False:
        app = participant.data.app

    if len(app) > 0:
    # add end time which we will need to calculate features
        end_time = []
        for i in range(0,app.shape[0]):
            durationN = app["duration"][i]
            start_time = app.index[i]
            end_time2 = app.index[i] + timedelta(seconds=int(durationN))
            end_time.append(end_time2)

        app["end_time"] = end_time

        app.genre.fillna('unknown', inplace=True)

        # Create a dataset that shows which app someone was using per second
        df1 = pd.DataFrame([(z, w) for x, y, w in zip(app.index, 
                                                          app['end_time'] - pd.Timedelta(1, unit='s'), 
                                                          app['package_name']) for z in pd.date_range(x, y, freq='s')], 
                               columns=['Date','Package']) 

        # Do the same for Genre
        df1b = pd.DataFrame([(z, w) for x, y, w in zip(app.index, 
                                                          app['end_time'] - pd.Timedelta(1, unit='s'), 
                                                          app['genre']) for z in pd.date_range(x, y, freq='s')], 
                               columns=['Date','Genre']) 

        # Merge both dataframes
        df1 = pd.merge(df1, df1b, how = 'left')

    ### Screen
    screen = participant.data.screen
    screen = screen.reset_index()

    ### Noise
    noise = participant.data.noise

    ### Ambient light
    light = participant.data.ambientlight
    light = light.reset_index()

    ### Power status
    power = participant.data.powerstatus

    ### Awareness
    awareness = participant.data.awareness

    ### Wifi
    wifi = participant.data.wifi
    wifi = wifi.reset_index()
    wifi['recorded_naive'] = pd.to_datetime(wifi['recorded_naive'])


    ### Bluetooth
    bluetooth = participant.data.bluetooth
    bluetooth = bluetooth.reset_index()
    bluetooth['recorded_naive'] = pd.to_datetime(bluetooth['recorded_naive'])

    ### SMS
    sms = participant.data.sms
    sms = sms.reset_index()

    # Here we create counters as helper for the foor loop

    counter1 = 0
    counter2 = 0
    counter3 = 0
    counter4 = 0
    counter5 = 0
    counter6 = 0
    counter7 = 0
    counter8 = 0 

    # Now we loop through each time point from the ESM questionnaires (stored in ESM_time) and calculate the featuers in relation to our chosen temporal resolution

    for i in range(0,ESM_time.shape[0]-1):
        time = ESM_time.loc[i,'index_time'] #get time from ESM questionnaire

        ########## APP USAGE ##########
        ###############################

        if len(app) > 0:
            # next we select the rows that are between x hours before the esm questionnaire (time - time delta) but not after the questionnaire is filled out
            HourAppDf = df1[(df1['Date'] > (time - timedelta(hours = rule_int))) & (df1['Date'] < time)].groupby(['Package']).size().div(60).to_frame('TotalMinutes')
            HourAppDf = HourAppDf.reset_index()

            if HourAppDf.shape[0] > 0:
                counter1 = counter1 + 1

                HourAppDf['Date'] = time
                HourAppDf['Package_min'] = HourAppDf.Package + "_min"

                HourAppDf = HourAppDf.pivot_table(index=['Date'], columns= 'Package_min', values='TotalMinutes').reset_index()

                #Time spent on all apps
                HourAppDf['APP_USAGE_min'] = HourAppDf.sum(axis =1)

                #Number of time opened all apps opened
                HourAppDf['APPS_OPENED_number'] = app[(app['end_time'] > (time - timedelta(hours = rule_int))) & (app['end_time'] < time)].shape[0]


                Genre = df1[(df1['Date'] > (time - timedelta(hours = rule_int))) & (df1['Date'] < time)].groupby(['Genre']).size().div(60).to_frame('Minutes')
                Genre = Genre.reset_index()

                Genre["Date"] = time

                Genre['Genre_min'] = Genre.Genre + "_min"

                ReShapeGenre = Genre.pivot(index=['Date'], columns='Genre_min', values='Minutes').reset_index()


                if HourAppDf.shape[0] > 0:
                    FeaturesCompleteApps = pd.merge(HourAppDf, ReShapeGenre, how = 'outer') 

                    if counter1 == 1:
                        FeatureOverviewApps = FeaturesCompleteApps
                    if counter1 != 1:
                        #merge different ESM time points
                        FeatureOverviewApps = pd.merge(FeatureOverviewApps, FeaturesCompleteApps, how = 'outer')   



                ######### Location #############
                ################################
                if len(staypoints) > 0:

                    # again we select the rows that are between x hours before the esm questionnaire (time - time delta) but not after the questionnaire is filled out
                    HourLocationDF = df2[(df2['Date'] > (time - timedelta(hours = rule_int))) & (df2['Date'] < time)].groupby(['Cluster']).size().div(60).to_frame('TotalMinutes')
                    HourLocationDF = HourLocationDF.reset_index()

                    HourLocationDF['Date'] = time
                    HourLocationDF['Cluster_min'] = "Cluster_" +  HourLocationDF.Cluster.astype('str') + "_min"

                    FeaturesLocation = HourLocationDF.pivot_table(index=['Date'], columns= 'Cluster_min', values='TotalMinutes').reset_index()


                    #Time spent stationary
                    TIME_STATIONARY_min = FeaturesLocation.sum(axis =1)

                    if len(TIME_STATIONARY_min) > 0:
                        TIME_STATIONARY_min = TIME_STATIONARY_min[0]

                    else:
                        TIME_STATIONARY_min = 0

                    #Number of staypoints
                    # Columns: Date has to be substracte, That's why I do "-1"
                    UNIQUE_STAYPOINTS_number = pd.notnull(FeaturesLocation.loc[:,"Date":]).sum(axis=1) - 1
                    if len(UNIQUE_STAYPOINTS_number) > 0:
                        UNIQUE_STAYPOINTS_number = UNIQUE_STAYPOINTS_number[0]
                    else:
                        UNIQUE_STAYPOINTS_number = 0

                    if FeaturesLocation.shape[0] > 0:
                        counter2 = counter2 + 1

                        if counter2 == 1:
                            FeatureOverviewLocation = FeaturesLocation
                            FeatureOverviewLocation['TIME_STATIONARY_min'] = TIME_STATIONARY_min
                            FeatureOverviewLocation['UNIQUE_STAYPOINTS_number'] = UNIQUE_STAYPOINTS_number


                        if counter2 != 1:
                            #merge different ESM time points
                            FeatureOverviewLocation = pd.merge(FeatureOverviewLocation, FeaturesLocation, how = 'outer')
                            FeatureOverviewLocation['TIME_STATIONARY_min'][counter2-1] = TIME_STATIONARY_min
                            FeatureOverviewLocation['UNIQUE_STAYPOINTS_number'][counter2-1] = UNIQUE_STAYPOINTS_number

                ######## Wifi ###########
                #########################

                if len(wifi) > 0:
                    counter3 = counter3 + 1

                    HourFeaturesWifi = pd.DataFrame({"Date": [time]})

                    # again we select the rows that are between x hours before the esm questionnaire (time - time delta) but not after the questionnaire is filled out
                    HourFeaturesWifi["TOTAL_MACHASHES_number"] = wifi[(wifi['recorded_naive'] > (time - timedelta(hours = rule_int))) & (wifi['recorded_naive'] < time)].shape[0]
                    HourFeaturesWifi["UNIQUE_MACHASHES_number"] = wifi[(wifi['recorded_naive'] > (time - timedelta(hours = rule_int))) & (wifi['recorded_naive'] < time)].mac_hash.unique().shape[0]

                    if HourFeaturesWifi.shape[0] > 0:

                        if counter3 == 1:
                            FeatureOverviewWifi = HourFeaturesWifi

                        if counter3 != 1:
                            #merge different ESM time points
                            FeatureOverviewWifi = pd.merge(FeatureOverviewWifi, HourFeaturesWifi, how = 'outer')   

               ######## Bluetooth ######
               #########################

                if len(bluetooth) > 0:
                    counter4 = counter4 + 1

                    HourFeaturesBlue = pd.DataFrame({"Date": [time]})
                    HourFeaturesBlue["BLUETOOTH_TOTAL_MACHASHES_number"] = bluetooth[(bluetooth['recorded_naive'] > (time - timedelta(hours = rule_int))) & (bluetooth['recorded_naive'] < time)].shape[0]
                    HourFeaturesBlue["BLUETOOTH_UNIQUE_MACHASHES_number"] = bluetooth[(bluetooth['recorded_naive'] > (time - timedelta(hours = rule_int))) & (bluetooth['recorded_naive'] < time)].mac_hash.unique().shape[0]

                    if HourFeaturesBlue.shape[0] > 0:
                        if counter4 == 1:
                            FeatureOverviewBlue = HourFeaturesBlue

                        if counter4 != 1:
                            FeatureOverviewBlue = pd.merge(FeatureOverviewBlue, HourFeaturesBlue, how = 'outer')   


                ###### Calls #######
                ####################

                if len(calld) > 0:
                    counter5 = counter5 + 1

                    CallFeatures = pd.DataFrame({"Date": [time]})
                    CallFeatures['CALL_TOTAL_min'] = int(df3[(df3['Date'] > (time - timedelta(hours = rule_int))) & (df3['Date'] < time)].shape[0])/60
                    CallFeatures['CALL_incoming_min'] = int(df3[(df3['Date'] > (time - timedelta(hours = rule_int))) & (df3['Date'] < time) & (df3["type"] == "incoming")].shape[0])/60
                    CallFeatures['CALL_outgoing_min'] = int(df3[(df3['Date'] > (time - timedelta(hours = rule_int))) & (df3['Date'] < time) & (df3["type"] == "outgoing")].shape[0])/60

                # Number of calls
                    CallFeatures['MISSED_CALLS_number'] = int(calld[(calld['recorded_naive'] > (time - timedelta(hours = rule_int))) & (calld['recorded_naive'] < time) & (calld["type"] == 'missed')].shape[0])
                    CallFeatures['CALL_TOTAL_number'] = int(calld[(calld['recorded_naive'] > (time - timedelta(hours = rule_int))) & (calld['recorded_naive'] < time)].shape[0])
                    CallFeatures['CALL_incoming_number'] = int(calld[(calld['recorded_naive'] > (time - timedelta(hours = rule_int))) & (calld['recorded_naive'] < time) & (calld["type"] == "incoming")].shape[0])
                    CallFeatures['CALL_outgoing_number']= int(calld[(calld['recorded_naive'] > (time - timedelta(hours = rule_int))) & (calld['recorded_naive'] < time) & (calld["type"] == "outgoing")].shape[0])

                    CallFeatures['CALL_UNIQUE_CONTACTS_number'] = int(calld[(calld['recorded_naive'] > (time - timedelta(hours = rule_int))) & (calld['recorded_naive'] < time)].caller_hash.unique().shape[0])

                    if len(CallFeatures) > 0:                  
                        if counter5 == 1:
                            FeatureOverviewCall = CallFeatures

                        if counter5 != 1:
                            #merge different ESM time points
                            FeatureOverviewCall = pd.merge(FeatureOverviewCall, CallFeatures, how = 'outer')   


                ###### Light #######
                ####################

                if len(light) > 0:
                    counter6 = counter6 + 1

                    LightFeatures = pd.DataFrame({"Date": [time]})

                    LightFeatures['LIGHT_LUX_mean'] = numpy.mean(light.loc[(light['recorded_naive'] > (time - timedelta(hours = rule_int))) & (light['recorded_naive'] < time), "lux"])
                    LightFeatures['LIGHT_LUX_std'] =numpy.std(light.loc[(light['recorded_naive'] > (time - timedelta(hours = rule_int))) & (light['recorded_naive'] < time), "lux"])



                    if len(LightFeatures) > 0:                  
                        if counter6 == 1:
                            FeatureOverviewLight = LightFeatures

                        if counter6 != 1:
                            #merge different ESM time points
                            FeatureOverviewLight = pd.merge(FeatureOverviewLight, LightFeatures, how = 'outer')   


                ###### SMS #######
                ####################

                if len(sms) > 0:
                    counter7 = counter7 + 1

                    SMSFeatures = pd.DataFrame({"Date": [time]})

                    SMSFeatures['SMS_received_number'] = int(sms[(sms['recorded_naive'] > (time - timedelta(hours = rule_int))) & (sms['recorded_naive'] < time) & (sms["type"] == "received")].shape[0])
                    SMSFeatures['SMS_sent_number']= int(sms[(sms['recorded_naive'] > (time - timedelta(hours = rule_int))) & (sms['recorded_naive'] < time) & (sms["type"] == "sent")].shape[0])

                    SMSFeatures['SMS_UNIQUE_CONTACTS_number'] = int(sms[(sms['recorded_naive'] > (time - timedelta(hours = rule_int))) & (sms['recorded_naive'] < time)].caller_hash.unique().shape[0])


                    if len(sms) > 0:                  
                        if counter7 == 1:
                             FeatureOverviewSMS = SMSFeatures

                        if counter7 != 1:
                            #merge different ESM time points
                            FeatureOverviewSMS = pd.merge(FeatureOverviewSMS, SMSFeatures, how = 'outer')   

                ###### Screen #######
                ####################

                if len(screen) > 0:
                    counter8 = counter8 + 1

                    ScreenFeatures = pd.DataFrame({"Date": [time]})

                    ScreenFeatures['SCREEN_onLocked_number'] = int(screen[(screen['recorded_naive'] > (time - timedelta(hours = rule_int))) & (screen['recorded_naive'] < time) & (screen["status"] == "onLocked")].shape[0])
                    ScreenFeatures['SCREEN_onUnlocked_number']= int(screen[(screen['recorded_naive'] > (time - timedelta(hours = rule_int))) & (screen['recorded_naive'] < time) & (screen["status"] == "onUnlocked")].shape[0])



                    if len(screen) > 0:                  
                        if counter8 == 1:
                             FeatureOverviewScreen = ScreenFeatures

                        if counter8 != 1:
                            #merge different ESM time points
                            FeatureOverviewScreen = pd.merge(FeatureOverviewScreen, ScreenFeatures, how = 'outer')   

    ############# Labeling Missing Values #############
    ###################################################
    
    #Here we label missing values per sensor. For each day we check if more than X (X = Missing_time) subsequent hours are without recored data. If so, we exclude all (ESM) data from that day.
    
    #****** APP USAGE *******
    FeatureOverviewApps = pd.merge(FeatureOverviewApps,ESM_time['Date'], on = ['Date'], how = 'right')

    ### Create an index whether or not to label an hour as NA
    #Show AppUsage per hour to check how many subsequent hours we have during the day in which no app usage was reported
    Index_App = (df1.groupby([df1['Date'].dt.date.rename('Date'), 
                                   df1['Date'].dt.hour.rename('Time'), df1['Package'], df1['Genre']]) 
                      .size().div(60).div(60).reset_index(name='APP_USAGE_pct'))
    Index_App = Index_App.groupby(['Date','Time'], axis=0, as_index=False).sum()

    Index_App["Difference_00"] = Index_App["Time"] # as a reference to calculate hours from 00:00 to first measurement and from last measurement to 00:00

    ### Calculate the time difference between each recorded app usage (to check how many subsequent hours exist without app usage)
    # Create time index
    Index_App["Date"] = pd.to_datetime(Index_App["Date"])
    Index_App["Time"] = pd.to_timedelta(Index_App["Time"], unit = "h")
    Index_App["index_time"] = Index_App["Date"] + Index_App["Time"]

    ## Check Direction 1 (for the following hours)

    Index_App["Difference"] = abs((Index_App['index_time']).groupby(Index_App['Date']).diff(1).dt.total_seconds().div(60).div(60)) # Grouped by day

    Index_App.loc[np.isnan(Index_App.loc[:,"Difference"]),"Difference"] = Index_App["Difference_00"] # Add difference from 00:00 to the first recorded hour of the day (because we don't always have app usage at 00:00)

    Index_App.loc[0,"Difference"] = 0 # Label very first day as 0

    #Create an index whether or not the difference is more than X hours (X = Missing_Time)
    Index_App["Difference_index"] = 0
    Index_App["Difference_index"][abs(Index_App["Difference"]) > Missing_Time] = 1

    ## Check Direction 2 (for the last hours)

    Index_App["Difference"] = abs((Index_App['index_time']).groupby(Index_App['Date']).diff(-1).dt.total_seconds().div(60).div(60)) # same just that we go in the other direction

    Index_App.loc[np.isnan(Index_App.loc[:,"Difference"]),"Difference"] = 24 - Index_App["Difference_00"] #Add difference from last measurement to 24:00 (because we don't always have app usage at 00:00)

    Index_App.loc[Index_App.shape[0],"Difference"] = 0 # Label last day as 0

    # Add a one to the index
    Index_App["Difference_index"][abs(Index_App["Difference"]) > Missing_Time] = Index_App["Difference_index"] + 1

    ### Group by date
    Index_App = Index_App.groupby(Index_App['Date']).sum().reset_index() 

    ### Label Missing Values
    FeatureOverviewApps = FeatureOverviewApps.fillna(0)

    #Check if Difference_index is 0 (Thus, we also check for days that are completly without any data) 
    for i in range(FeatureOverviewApps.shape[0]):
        if  pd.to_datetime(FeatureOverviewApps['Date'].dt.date)[i] not in pd.to_datetime(Index_App['Date'][Index_App["Difference_index"] == 0]).to_list():
            FeatureOverviewApps.iloc[i,1:] = float("NaN")  


    #****** Location *******
    FeatureOverviewLocation = pd.merge(FeatureOverviewLocation,ESM_time['Date'], on = ['Date'], how = 'right')

    ### Create an index whether or not to label an hour as NA
    #Show AppUsage per hour to check how many subsequent hours we have during the day in which no app usage was reported
    Index_Loc = locraw.groupby([locraw['index_time'].dt.date.rename('Date'),locraw['index_time'].dt.hour.rename('Time')]).size().reset_index(name = 'Number')
    Index_Loc = Index_Loc.groupby(['Date','Time'], axis=0, as_index=False).sum()

    Index_Loc["Difference_00"] = Index_Loc["Time"] # as a reference to calculate hours from 00:00 to first measurement and from last measurement to 00:00

    ### Calculate the time difference between each recorded app usage (to check how many subsequent hours exist without app usage)
    # Create time index
    Index_Loc["Date"] = pd.to_datetime(Index_Loc["Date"])
    Index_Loc["Time"] = pd.to_timedelta(Index_Loc["Time"], unit = "h")
    Index_Loc["index_time"] = Index_Loc["Date"] + Index_Loc["Time"]

    ## Check Direction 1 (for the following hours)

    Index_Loc["Difference"] = abs((Index_Loc['index_time']).groupby(Index_Loc['Date']).diff(1).dt.total_seconds().div(60).div(60)) # Grouped by day

    Index_Loc.loc[np.isnan(Index_Loc.loc[:,"Difference"]),"Difference"] = Index_Loc["Difference_00"] # Add difference from 00:00 to the first recorded hour of the day (because we don't always have app usage at 00:00)

    Index_Loc.loc[0,"Difference"] = 0 # Label very first day as 0

    #Create an index whether or not the difference is more than X hours (X = Missing_Time)
    Index_Loc["Difference_index"] = 0
    Index_Loc["Difference_index"][abs(Index_Loc["Difference"]) > Missing_Time] = 1

    ## Check Direction 2 (for the last hours)

    Index_Loc["Difference"] = abs((Index_Loc['index_time']).groupby(Index_Loc['Date']).diff(-1).dt.total_seconds().div(60).div(60)) # same just that we go in the other direction

    Index_Loc.loc[np.isnan(Index_Loc.loc[:,"Difference"]),"Difference"] = 24 - Index_Loc["Difference_00"] #Add difference from last measurement to 24:00 (because we don't always have app usage at 00:00)

    Index_Loc.loc[Index_Loc.shape[0],"Difference"] = 0 # Label last day as 0

    # Add a one to the index
    Index_Loc["Difference_index"][abs(Index_Loc["Difference"]) > Missing_Time] = Index_Loc["Difference_index"] + 1

    ### Group by date
    Index_Loc = Index_Loc.groupby(Index_Loc['Date']).sum().reset_index() 

    ### Label Missing Values
    FeatureOverviewLocation = FeatureOverviewLocation.fillna(0)

    #Check if Difference_index is 0 (Thus, we also check for days that are completly without any data) 
    for i in range(FeatureOverviewLocation.shape[0]):
        if  pd.to_datetime(FeatureOverviewLocation['Date'].dt.date)[i] not in pd.to_datetime(Index_Loc['Date'][Index_Loc["Difference_index"] == 0]).to_list():
            FeatureOverviewLocation.iloc[i,1:] = float("NaN")  


    #****** Wifi *******
    FeatureOverviewWifi = pd.merge(FeatureOverviewWifi,ESM_time['Date'], on = ['Date'], how = 'right')

    ### Create an index whether or not to label an hour as NA
    #Show AppUsage per hour to check how many subsequent hours we have during the day in which no app usage was reported
    Index_wifi = wifi.groupby([wifi['recorded_naive'].dt.date.rename('Date'),wifi['recorded_naive'].dt.hour.rename('Time'),wifi['mac_hash']]).size().reset_index(name = 'UNIQUE_MACHASHES_norm')

    Index_wifi = Index_wifi.groupby(['Date','Time'], axis=0, as_index=False).sum()

    Index_wifi["Difference_00"] = Index_wifi["Time"] # as a reference to calculate hours from 00:00 to first measurement and from last measurement to 00:00

    ### Calculate the time difference between each recorded app usage (to check how many subsequent hours exist without app usage)
    # Create time index
    Index_wifi["Date"] = pd.to_datetime(Index_wifi["Date"])
    Index_wifi["Time"] = pd.to_timedelta(Index_wifi["Time"], unit = "h")
    Index_wifi["index_time"] = Index_wifi["Date"] + Index_wifi["Time"]

    ## Check Direction 1 (for the following hours)

    Index_wifi["Difference"] = abs((Index_wifi['index_time']).groupby(Index_wifi['Date']).diff(1).dt.total_seconds().div(60).div(60)) # Grouped by day

    Index_wifi.loc[np.isnan(Index_wifi.loc[:,"Difference"]),"Difference"] = Index_wifi["Difference_00"] # Add difference from 00:00 to the first recorded hour of the day (because we don't always have app usage at 00:00)

    Index_wifi.loc[0,"Difference"] = 0 # Label very first day as 0

    #Create an index whether or not the difference is more than X hours (X = Missing_Time)
    Index_wifi["Difference_index"] = 0
    Index_wifi["Difference_index"][abs(Index_wifi["Difference"]) > Missing_Time] = 1

    ## Check Direction 2 (for the last hours)

    Index_wifi["Difference"] = abs((Index_wifi['index_time']).groupby(Index_wifi['Date']).diff(-1).dt.total_seconds().div(60).div(60)) # same just that we go in the other direction

    Index_wifi.loc[np.isnan(Index_wifi.loc[:,"Difference"]),"Difference"] = 24 - Index_wifi["Difference_00"] #Add difference from last measurement to 24:00 (because we don't always have app usage at 00:00)

    Index_wifi.loc[Index_wifi.shape[0],"Difference"] = 0 # Label last day as 0

    # Add a one to the index
    Index_wifi["Difference_index"][abs(Index_wifi["Difference"]) > Missing_Time] = Index_wifi["Difference_index"] + 1

    ### Group by date
    Index_wifi = Index_wifi.groupby(Index_wifi['Date']).sum().reset_index() 

    ### Label Missing Values
    FeatureOverviewWifi = FeatureOverviewWifi.fillna(0)

    #Check if Difference_index is 0 (Thus, we also check for days that are completly without any data) 
    for i in range(FeatureOverviewWifi.shape[0]):
        if  pd.to_datetime(FeatureOverviewWifi['Date'].dt.date)[i] not in pd.to_datetime(Index_wifi['Date'][Index_wifi["Difference_index"] == 0]).to_list():
            FeatureOverviewWifi.iloc[i,1:] = float("NaN")  
  

    #****** Bluetooth *******
    if len(bluetooth) > 0:
        FeatureOverviewBlue = pd.merge(FeatureOverviewBlue,ESM_time['Date'], on = ['Date'], how = 'right')
        
        ### Create an index whether or not to label an hour as NA
        #Show AppUsage per hour to check how many subsequent hours we have during the day in which no app usage was reported
        Index_Blue = bluetooth.groupby([bluetooth['recorded_naive'].dt.date.rename('Date'),bluetooth['recorded_naive'].dt.hour.rename('Time'),bluetooth['mac_hash']]).size().reset_index(name = 'UNIQUE_MACHASHES_norm')

        Index_Blue = Index_Blue.groupby(['Date','Time'], axis=0, as_index=False).sum()

        Index_Blue["Difference_00"] = Index_Blue["Time"] # as a reference to calculate hours from 00:00 to first measurement and from last measurement to 00:00

        ### Calculate the time difference between each recorded app usage (to check how many subsequent hours exist without app usage)
        # Create time index
        Index_Blue["Date"] = pd.to_datetime(Index_Blue["Date"])
        Index_Blue["Time"] = pd.to_timedelta(Index_Blue["Time"], unit = "h")
        Index_Blue["index_time"] = Index_Blue["Date"] + Index_Blue["Time"]

        ## Check Direction 1 (for the following hours)

        Index_Blue["Difference"] = abs((Index_Blue['index_time']).groupby(Index_Blue['Date']).diff(1).dt.total_seconds().div(60).div(60)) # Grouped by day

        Index_Blue.loc[np.isnan(Index_Blue.loc[:,"Difference"]),"Difference"] = Index_Blue["Difference_00"] # Add difference from 00:00 to the first recorded hour of the day (because we don't always have app usage at 00:00)

        Index_Blue.loc[0,"Difference"] = 0 # Label very first day as 0

        #Create an index whether or not the difference is more than X hours (X = Missing_Time)
        Index_Blue["Difference_index"] = 0
        Index_Blue["Difference_index"][abs(Index_Blue["Difference"]) > Missing_Time] = 1

        ## Check Direction 2 (for the last hours)

        Index_Blue["Difference"] = abs((Index_Blue['index_time']).groupby(Index_Blue['Date']).diff(-1).dt.total_seconds().div(60).div(60)) # same just that we go in the other direction

        Index_Blue.loc[np.isnan(Index_Blue.loc[:,"Difference"]),"Difference"] = 24 - Index_Blue["Difference_00"] #Add difference from last measurement to 24:00 (because we don't always have app usage at 00:00)

        Index_Blue.loc[Index_Blue.shape[0],"Difference"] = 0 # Label last day as 0

        # Add a one to the index
        Index_Blue["Difference_index"][abs(Index_Blue["Difference"]) > Missing_Time] = Index_Blue["Difference_index"] + 1

        ### Group by date
        Index_Blue = Index_Blue.groupby(Index_Blue['Date']).sum().reset_index() 

        ### Label Missing Values
        FeatureOverviewBlue = FeatureOverviewBlue.fillna(0)

        #Check if Difference_index is 0 (Thus, we also check for days that are completly without any data) 
        for i in range(FeatureOverviewBlue.shape[0]):
            if  pd.to_datetime(FeatureOverviewBlue['Date'].dt.date)[i] not in pd.to_datetime(Index_Blue['Date'][Index_Blue["Difference_index"] == 0]).to_list():
                FeatureOverviewBlue.iloc[i,1:] = float("NaN")  


    #****** Call *******

    if len(calld) > 0:
        FeatureOverviewCall = pd.merge(FeatureOverviewCall,ESM_time['Date'], on = ['Date'], how = 'right')

    else:
        FeatureOverviewCall = pd.DataFrame({"CALL_TOTAL_min": 0},index = range(ESM_time.shape[0])) 

    FeatureOverviewCall = FeatureOverviewCall.fillna(0)

    #****** SMS ********

    if len(sms) > 0:
        FeatureOverviewSMS = pd.merge(FeatureOverviewSMS,ESM_time['Date'], on = ['Date'], how = 'right')

    else:
        FeatureOverviewSMS = pd.DataFrame({"CALL_TOTAL_min": 0},index = range(ESM_time.shape[0])) 

    FeatureOverviewSMS = FeatureOverviewSMS.fillna(0)

    #****** Light *******

    if len(light) > 0:
        
        FeatureOverviewLight = pd.merge(FeatureOverviewLight,ESM_time['Date'], on = ['Date'], how = 'right')
        
        ### Create an index whether or not to label an hour as NA
        #Show AppUsage per hour to check how many subsequent hours we have during the day in which no app usage was reported
        Index_Light = light.groupby([light['recorded_naive'].dt.date.rename('Date'),light['recorded_naive'].dt.hour.rename('Time')]).size().reset_index(name = 'Count')
        
        Index_Light = Index_Light.groupby(['Date','Time'], axis=0, as_index=False).sum()

        Index_Light["Difference_00"] = Index_Light["Time"] # as a reference to calculate hours from 00:00 to first measurement and from last measurement to 00:00

        ### Calculate the time difference between each recorded app usage (to check how many subsequent hours exist without app usage)
        # Create time index
        Index_Light["Date"] = pd.to_datetime(Index_Light["Date"])
        Index_Light["Time"] = pd.to_timedelta(Index_Light["Time"], unit = "h")
        Index_Light["index_time"] = Index_Light["Date"] + Index_Light["Time"]

        ## Check Direction 1 (for the following hours)

        Index_Light["Difference"] = abs((Index_Light['index_time']).groupby(Index_Light['Date']).diff(1).dt.total_seconds().div(60).div(60)) # Grouped by day

        Index_Light.loc[np.isnan(Index_Light.loc[:,"Difference"]),"Difference"] = Index_Light["Difference_00"] # Add difference from 00:00 to the first recorded hour of the day (because we don't always have app usage at 00:00)

        Index_Light.loc[0,"Difference"] = 0 # Label very first day as 0

        #Create an index whether or not the difference is more than X hours (X = Missing_Time)
        Index_Light["Difference_index"] = 0
        Index_Light["Difference_index"][abs(Index_Light["Difference"]) > Missing_Time] = 1

        ## Check Direction 2 (for the last hours)

        Index_Light["Difference"] = abs((Index_Light['index_time']).groupby(Index_Light['Date']).diff(-1).dt.total_seconds().div(60).div(60)) # same just that we go in the other direction

        Index_Light.loc[np.isnan(Index_Light.loc[:,"Difference"]),"Difference"] = 24 - Index_Light["Difference_00"] #Add difference from last measurement to 24:00 (because we don't always have app usage at 00:00)

        Index_Light.loc[Index_Light.shape[0],"Difference"] = 0 # Label last day as 0

        # Add a one to the index
        Index_Light["Difference_index"][abs(Index_Light["Difference"]) > Missing_Time] = Index_Light["Difference_index"] + 1

        ### Group by date
        Index_Light = Index_Light.groupby(Index_Light['Date']).sum().reset_index() 

        ### Label Missing Values
        FeatureOverviewLight = FeatureOverviewLight.fillna(0)

        #Check if Difference_index is 0 (Thus, we also check for days that are completly without any data) 
        for i in range(FeatureOverviewLight.shape[0]):
            if  pd.to_datetime(FeatureOverviewLight['Date'].dt.date)[i] not in pd.to_datetime(Index_Light['Date'][Index_Light["Difference_index"] == 0]).to_list():
                FeatureOverviewLight.iloc[i,1:] = float("NaN")  
    

    #****** Screen *******

    if len(screen) > 0:
        FeatureOverviewScreen = pd.merge(FeatureOverviewScreen,ESM_time['Date'], on = ['Date'], how = 'right')
        
        ### Create an index whether or not to label an hour as NA
        #Show AppUsage per hour to check how many subsequent hours we have during the day in which no app usage was reported
        Index_screen = screen.groupby([screen['recorded_naive'].dt.date.rename('Date'),screen['recorded_naive'].dt.hour.rename('Time')]).size().reset_index(name = 'Count')
        
        Index_screen = Index_screen.groupby(['Date','Time'], axis=0, as_index=False).sum()

        Index_screen["Difference_00"] = Index_screen["Time"] # as a reference to calculate hours from 00:00 to first measurement and from last measurement to 00:00

        ### Calculate the time difference between each recorded app usage (to check how many subsequent hours exist without app usage)
        # Create time index
        Index_screen["Date"] = pd.to_datetime(Index_screen["Date"])
        Index_screen["Time"] = pd.to_timedelta(Index_screen["Time"], unit = "h")
        Index_screen["index_time"] = Index_screen["Date"] + Index_screen["Time"]

        ## Check Direction 1 (for the following hours)

        Index_screen["Difference"] = abs((Index_screen['index_time']).groupby(Index_screen['Date']).diff(1).dt.total_seconds().div(60).div(60)) # Grouped by day

        Index_screen.loc[np.isnan(Index_screen.loc[:,"Difference"]),"Difference"] = Index_screen["Difference_00"] # Add difference from 00:00 to the first recorded hour of the day (because we don't always have app usage at 00:00)

        Index_screen.loc[0,"Difference"] = 0 # Label very first day as 0

        #Create an index whether or not the difference is more than X hours (X = Missing_Time)
        Index_screen["Difference_index"] = 0
        Index_screen["Difference_index"][abs(Index_screen["Difference"]) > Missing_Time] = 1

        ## Check Direction 2 (for the last hours)

        Index_screen["Difference"] = abs((Index_screen['index_time']).groupby(Index_screen['Date']).diff(-1).dt.total_seconds().div(60).div(60)) # same just that we go in the other direction

        Index_screen.loc[np.isnan(Index_screen.loc[:,"Difference"]),"Difference"] = 24 - Index_screen["Difference_00"] #Add difference from last measurement to 24:00 (because we don't always have app usage at 00:00)

        Index_screen.loc[Index_screen.shape[0],"Difference"] = 0 # Label last day as 0

        # Add a one to the index
        Index_screen["Difference_index"][abs(Index_screen["Difference"]) > Missing_Time] = Index_screen["Difference_index"] + 1

        ### Group by date
        Index_screen = Index_screen.groupby(Index_screen['Date']).sum().reset_index() 

        # Label Missing Values
        FeatureOverviewScreen = FeatureOverviewScreen.fillna(0)

        for i in range(FeatureOverviewScreen.shape[0]):
            if  pd.to_datetime(FeatureOverviewScreen['Date'].dt.date)[i] not in pd.to_datetime(Index_screen['Date'][Index_screen["Difference_index"] == 0]).to_list():
                FeatureOverviewScreen.iloc[i,1:] = float("NaN")  



    FeatureOverview =  pd.concat([FeatureOverviewApps,FeatureOverviewLocation,FeatureOverviewWifi, FeatureOverviewBlue, FeatureOverviewCall, FeatureOverviewLight, FeatureOverviewSMS, FeatureOverviewScreen], axis = 1)
    FeatureOverview["timescale_beforeESM"] = rule
    FeatureOverview["ParticipantNumber"] = participant.id
    FeatureOverview = FeatureOverview.loc[:,~FeatureOverview.columns.duplicated()]



    return FeatureOverview


# In[ ]:


# Test Function with one participant
start_time = time.time()
test = ESM_passivemeasures(participant = 117135,rule = '3h', rule_int = 3, app_cleaning = True, Missing_Time = 24)
end_time = time.time()


# In[ ]:


pd.set_option('display.max_rows', 10)
test


# In[ ]:


BEHAPPID = [117113,117114,117119,117121,117130,117129,117131,117134,117135,117137]
#BEHAPPID = [117119,117130,117134]


start_time = time.time()
counter = 0

Missing_Time = 24

for x in BEHAPPID:
    print(x)
    counter = counter + 1
    if counter == 1:
        Data1h = ESM_passivemeasures(participant = x,rule = '1h', rule_int = 1, app_cleaning = True, Missing_Time = Missing_Time)
        Data3h = ESM_passivemeasures(participant = x,rule = '3h', rule_int = 3, app_cleaning = True, Missing_Time = Missing_Time)
        Data6h = ESM_passivemeasures(participant = x,rule = '6h', rule_int = 6, app_cleaning = True, Missing_Time = Missing_Time)
        Data9h = ESM_passivemeasures(participant = x,rule = '9h', rule_int = 9, app_cleaning = True, Missing_Time = Missing_Time)
        Data12h = ESM_passivemeasures(participant = x,rule = '12h', rule_int = 12, app_cleaning = True, Missing_Time = Missing_Time)
        Data24h = ESM_passivemeasures(participant = x,rule = '24h', rule_int = 24, app_cleaning = True, Missing_Time = Missing_Time)
             
        FullData = pd.concat([Data1h, Data3h,Data6h, Data9h, Data12h, Data24h])
        
    if counter > 1:
        Data1h = ESM_passivemeasures(participant = x,rule = '1h', rule_int = 1, app_cleaning = True, Missing_Time = Missing_Time)
        Data3h = ESM_passivemeasures(participant = x,rule = '3h', rule_int = 3, app_cleaning = True, Missing_Time = Missing_Time)
        Data6h = ESM_passivemeasures(participant = x,rule = '6h', rule_int = 6, app_cleaning = True, Missing_Time = Missing_Time)
        Data9h = ESM_passivemeasures(participant = x,rule = '9h', rule_int = 9, app_cleaning = True, Missing_Time = Missing_Time)
        Data12h = ESM_passivemeasures(participant = x,rule = '12h', rule_int = 12, app_cleaning = True, Missing_Time = Missing_Time)
        Data24h = ESM_passivemeasures(participant = x,rule = '24h', rule_int = 24, app_cleaning = True, Missing_Time = Missing_Time)
             
        Data = pd.concat([Data1h, Data3h,Data6h, Data9h, Data12h, Data24h])
        
        FullData = pd.concat([FullData,Data]) #If a column doesn't appear it will be Na even though it should be 0


end_time = time.time()

FullData


# In[ ]:


FullData.to_csv("/Users/annalangener/Nextcloud/BEHAPP data/Data_PassiveMeasure2709_24hmissing.csv", sep=',', na_rep='NA')

