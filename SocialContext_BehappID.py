#!/usr/bin/env python
# coding: utf-8

# ### Social Context Data

# In[ ]:


import plotly.express as px
from jupyter_dash import JupyterDash
from dash import dcc
from dash import html
from dash.dependencies import Input, Output
from behapp import Participant 
import numpy as np
import time
import pandas as pd
from datetime import date, timedelta
import plotly as pl
import plotly.graph_objects as go
import traces
import datetime
import plotly.subplots as sp
import openpyxl
import dash_table
from behapp import Study


# In[ ]:


# Active Data
ESM = pd.read_csv('/Users/annalangener/Nextcloud/Shared/Testing Methods to Capture Social Context/Cleaning ESM/DataCleaned.csv', sep = ',', low_memory = False)

#Replace by BEHAPP_ID
BEHAPPID = ['NA', 117113, 117114, 'NA',117118, 117116, 117117, 117120,
       117122, 117119, 117121, 117123,117127, 117129, 117128, 117130,
       117131, 117140, 117139, 117138, 117136, 117134, 117135, 117137]

# mPath = Deleted to keep data anonymous

ESM['BEHAPP_ID'] = ESM['alias'].replace(mPath, BEHAPPID)

ESM = ESM[ESM['BEHAPP_ID'] != 'NA']

ESM['index_time'] = 0

for i in ESM.index:
    if ESM['timeStampStart'][i] > 0:
        ESM['index_time'][i] = time.strftime('%Y-%m-%d %H:%M:%S', time.gmtime(ESM['timeStampStart'][i]))

ESM["index_time"] = pd.to_datetime(ESM["index_time"])
ESM = ESM[ESM["timeStampStart"] > 0]

ESM = pd.read_csv('/Users/annalangener/Nextcloud/BEHAPP data/SocialContext.csv', sep=',')

