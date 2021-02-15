import pandas as pd
import numpy as np
import random
from sklearn.preprocessing import StandardScaler
import gc
import matplotlib.pyplot as plt
import pickle
import torch
from utils.preprocess_utils import *

# load data
time_data = pd.read_csv('data/original/time_data.txt', sep  = ' ') 
time_data = df2npy(time_data)

# 아침 점심 저녁 분리
morning_data = time_data[:,2,:,:]
lunch_data = time_data[:,1,:,:]
evening_data = time_data[:,0,:,:]

# split time data
# train_validation_test
# scaling
# make intput, output window
morning_data, train_valid_test_loc_index, m_time_scaler = split_train_valid_test(morning_data)
lunch_data, _, l_time_scaler = split_train_valid_test(lunch_data)
evening_data, _, e_time_scaler = split_train_valid_test(evening_data)

# split notime data
# notime_train, notime_valid, notime_test = notime
nontime_data = pd.read_csv('data/original/nontime_data.txt', sep = ' ')
notime, no_time_scaler = split_notime_data(nontime_data, train_valid_test_loc_index)

print('\n morning')
m_train_time, m_train_notime, m_train_y = make_data(morning_data[:2], notime)
m_valid_time, m_valid_notime, m_valid_y = make_data(morning_data[2], notime)
m_test_time, m_test_notime, m_test_y = make_data(morning_data[3], notime)

print('\n lunch')
l_train_time, l_train_notime, l_train_y = make_data(lunch_data[:2], notime)
l_valid_time, l_valid_notime, l_valid_y = make_data(lunch_data[2], notime)
l_test_time, l_test_notime, l_test_y = make_data(lunch_data[3], notime)

print('\n evening')
e_train_time, e_train_notime, e_train_y = make_data(evening_data[:2], notime)
e_valid_time, e_valid_notime, e_valid_y = make_data(evening_data[2], notime)
e_test_time, e_test_notime, e_test_y = make_data(evening_data[3], notime)


m_data_list = [m_train_time[:,:,:], m_train_notime, m_train_y, 
                m_valid_time[:,:,:], m_valid_notime, m_valid_y,
                m_test_time[:,:,:], m_test_notime, m_test_y]

l_data_list = [l_train_time[:,:,:], l_train_notime, l_train_y, 
                l_valid_time[:,:,:], l_valid_notime, l_valid_y,
                l_test_time[:,:,:], l_test_notime, l_test_y]

e_data_list = [e_train_time[:,:,:], e_train_notime, e_train_y, 
                e_valid_time[:,:,:], e_valid_notime, e_valid_y,
                e_test_time[:,:,:], e_test_notime, e_test_y]    

path = 'data/preprocess/'
with open(path + 'm_data_list.pkl', 'wb') as f:
    pickle.dump(m_data_list, f)
with open(path + 'l_data_list.pkl', 'wb') as f:
    pickle.dump(l_data_list, f)
with open(path + 'e_data_list.pkl', 'wb') as f:
    pickle.dump(e_data_list, f)


scalers = [m_time_scaler, l_time_scaler, e_time_scaler, no_time_scaler]
with open(path + 'scalers.pkl', 'wb') as f:
    pickle.dump(scalers,f)



## 학습한 모델을 기반으로 6월을 예측하기 위한 데이서 생성 code

nontime = pd.read_csv('data/original/nontime_data.txt', sep = ' ')
time = pd.read_csv('data/original/time_data.txt', sep = ' ')

# load data
time_data = pd.read_csv('data/original/time_data.txt', sep  = ' ') 
time_data = df2npy(time_data)
morning_data = time_data[:,2,:,:]
lunch_data = time_data[:,1,:,:]
evening_data = time_data[:,0,:,:]
morning_data, train_valid_test_loc_index, m_time_scaler = split_train_valid_test(morning_data)
lunch_data, _, l_time_scaler = split_train_valid_test(lunch_data)
evening_data, _, e_time_scaler = split_train_valid_test(evening_data)

nontime_data = pd.read_csv('data/original/nontime_data.txt', sep = ' ')
notime, no_time_scaler = split_notime_data(nontime_data, train_valid_test_loc_index)
code_list = evening_data[0][:,0,0]
def name2index(dong_name):
    for idx, code in enumerate(code_list):
        if code == time[time.HDONG_NM == dong_name].HDONG_CD.iloc[0]:
            return(idx)
def append_trend_cycle(flow_pop):
    new_flow_pop = np.zeros([len(flow_pop), 2])
    #new_flow_pop[:, 0] = seq2cycle(flow_pop)[ROLLSIZE:]
    new_flow_pop[:, 0] = seq2cycle_weight(flow_pop)
    new_flow_pop[:, 1] = flow_pop - new_flow_pop[:, 0]
    #print(new_flow_pop)
    return new_flow_pop
def split_sequence(sequence, target_index  = 2):
    seq_x = sequence[:, :]
    trend_cycle_x = append_trend_cycle(seq_x[:,target_index])
    seq_x = np.concatenate([seq_x[ROLLSIZE:,:],
                            trend_cycle_x[ROLLSIZE:]], axis = 1)
    return seq_x

path = 'data/predict_june/preprocess_june/'

time_data = morning_data[3][:,-24:,:]
notime_data = notime
x_time = []
x_notime = []

for loc in range(len(time_data)):
    loc_code = time_data[loc,0,0]
    x = split_sequence(time_data[loc,:,:])
    x_time.append(x.reshape(1, x.shape[0], x.shape[1]))
    no_time = notime_data.loc[loc_code]
    aug_notime = np.zeros(3)
    aug_notime[:] = no_time
    x_notime.append(aug_notime.reshape(1,-1))
x_time = np.concatenate(x_time)
x_notime = np.concatenate(x_notime)


with open(path + 'morning_june_time.pkl', 'wb') as f:
    pickle.dump(x_time, f)
with open(path + 'morning_june_notime.pkl', 'wb') as f:
    pickle.dump(x_notime, f)

time_data = lunch_data[3][:,-24:,:]
notime_data = notime
x_time = []
x_notime = []

for loc in range(len(time_data)):
    loc_code = time_data[loc,0,0]
    x = split_sequence(time_data[loc,:,:])
    x_time.append(x.reshape(1, x.shape[0], x.shape[1]))
    no_time = notime_data.loc[loc_code]
    aug_notime = np.zeros(3)
    aug_notime[:] = no_time
    x_notime.append(aug_notime.reshape(1,-1))

x_time = np.concatenate(x_time)
x_notime = np.concatenate(x_notime)


with open(path + 'lunch_june_time.pkl', 'wb') as f:
    pickle.dump(x_time, f)
with open(path + 'lunch_june_notime.pkl', 'wb') as f:
    pickle.dump(x_notime, f)

time_data = evening_data[3][:,-24:,:]
notime_data = notime
x_time = []
x_notime = []

for loc in range(len(time_data)):
    loc_code = time_data[loc,0,0]
    x = split_sequence(time_data[loc,:,:])
    x_time.append(x.reshape(1, x.shape[0], x.shape[1]))
    no_time = notime_data.loc[loc_code]
    aug_notime = np.zeros(3)
    aug_notime[:] = no_time
    x_notime.append(aug_notime.reshape(1,-1))

x_time = np.concatenate(x_time)
x_notime = np.concatenate(x_notime)


with open(path + 'evening_june_time.pkl', 'wb') as f:
    pickle.dump(x_time, f)
with open(path + 'evening_june_notime.pkl', 'wb') as f:
    pickle.dump(x_notime, f)

print("complete make june data")