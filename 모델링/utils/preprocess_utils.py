import pandas as pd
import numpy as np
import random
from sklearn.preprocessing import StandardScaler
import gc
import matplotlib.pyplot as plt
import pickle
import torch
# define 'device' to upload tensor in gpu
device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

# set constant
LOC_SIZE = 69
TIME_SIZE = 3
DATE_SIZE = 241
FEATURE_SIZE = 13

# set constant
LOC_SIZE = 69
TIME_SIZE = 3
DATE_SIZE = 241
FEATURE_SIZE = 13
#set window size
INPUT_WINDOW = 21
OUTPUT_WINDOW = 7
ROLLSIZE = 3

class StandardScalerSelect(StandardScaler):
    def __init__(self, copy=True, with_mean=True, with_std=True):
        self.shape = None
        super().__init__(copy, with_mean, with_std)
    def fit(self, X):
        self.shape = X.shape
        super().fit(X)
    def inverse_transform(self, X, select_col = None):
        if select_col != None:
            temp_X = np.zeros([X.shape[0], self.shape[1]])
            temp_X[:,select_col] = X
            trans = super().inverse_transform(temp_X)
            return trans[:,select_col]
        else:
            trans = super().inverse_transform(X)
            return trans
    def transform(self, X, select_col = None):
        if select_col != None:
            temp_X = np.zeros([X.shape[0], self.shape[1]])
            temp_X[:,select_col] = X
            trans = super().transform(temp_X)
            return trans[:,select_col]
        else:
            trans = super().transform(X)
            return trans


def df2npy(time_data):
    # make loc_list(dong code)
    loc_list = list(time_data.HDONG_CD.unique())

    # select features
    time_data = time_data[['flow_pop', 'HDONG_CD', 'time',
                        'card_use', 'holiday', 'day_corona', 'ondo', 'subdo',
                        'rain_snow', 'STD_YMD']]
    # change string time to int time
    time_data.time[time_data.time == 'morning'] = 0 # morning
    time_data.time[time_data.time == 'lunch'] = 1 # lunch
    time_data.time[time_data.time == 'evening'] = 2 # evening

    # to datetime
    time_data.STD_YMD = pd.to_datetime(time_data.STD_YMD)

    # make dayofyear weekday
    time_data['dayofyear'] = time_data.STD_YMD.dt.dayofyear
    time_data['weekday'] = time_data.STD_YMD.dt.weekday
    time_data['dayofyear_sin'] = np.sin(2 * np.pi * (time_data['dayofyear'])/365)
    time_data['dayofyear_cos'] = np.cos(2 * np.pi * (time_data['dayofyear'])/365)
    time_data['weekday_sin'] = np.sin(2 * np.pi * (time_data['weekday'])/7)# 월화수목금토일
    time_data['weekday_cos'] = np.cos(2 * np.pi * (time_data['weekday'])/7)

    # reselect features
    time_data = time_data[['HDONG_CD', 'time','flow_pop',
                        'card_use', 'holiday', 'day_corona', 'ondo', 'subdo',
                        'rain_snow', 'dayofyear_sin', 'dayofyear_cos', 'weekday_sin', 'weekday_cos']]
    # table -> matrix
    time_data = np.array(time_data).reshape(LOC_SIZE, TIME_SIZE, DATE_SIZE, FEATURE_SIZE)# 지역, 시간, 날짜, features
    return time_data

def scaleing_time(data, scaler = None):
    shape = data.shape
    data = data.reshape(-1, shape[-1])
    if scaler == None:
        scaler = StandardScalerSelect()
        data_idx = np.arange(len(data))
        np.random.seed(1015)
        np.random.shuffle(data_idx)
        scaler.fit(data[:data_idx[int(data.shape[0]-1)]])###################################
    scaled_data = scaler.transform(data)
    return scaler, scaled_data.reshape(shape)

def scaleing_no_time(data, scaler = None):
    df_index = data.index
    df_columns = data.columns
    data = data.values
    if scaler == None:
        scaler = StandardScalerSelect()

        data_idx = np.arange(len(data))
        np.random.seed(1015)
        np.random.shuffle(data_idx)
        scaler.fit(data[:data_idx[int(data.shape[0])-1]])################################### 
    
    data = scaler.transform(data)
    data = pd.DataFrame(data, index = df_index, columns = df_columns)
    return scaler, data

def split_train_valid_test(time_data, scaler = None):
    # make_random 
    loc_index = [i for i in range(69)]
    random.seed(1015)
    random.shuffle(loc_index)

    # split time data
    # 0일 ~ 119일 -> 19년
    # 120일 ~ -> 20년
    train_time_19 = time_data[loc_index[ : 69], : 120, :]
    train_time_20 = time_data[loc_index[ : 69], 120 : 201, :]

    
    valid_time_1 = time_data[loc_index[ : 69], (201 - (INPUT_WINDOW + ROLLSIZE*2 + OUTPUT_WINDOW - 1) ) : 221, :] # train 지역& valid 기간
    
    #valid_time_2_19 = time_data[loc_index[55:62], : 119, :]
    #valid_time_2_20 = time_data[loc_index[55:62], (119 - ROLLSIZE) : -20, :] # valid 지역 & (train + valid) 기간
    
    test_time_1 = time_data[loc_index[ : 69], 221- (INPUT_WINDOW + ROLLSIZE*2 + OUTPUT_WINDOW-1) : , :] # train,valid 지역& test 기간
    #test_time_2_19 = time_data[loc_index[62:], :119, :] # test 지역 & (train + valid + test) 기간
    #test_time_2_20 = time_data[loc_index[62:], (119 - ROLLSIZE):, :]

    # set loc index
    train_loc_index = list(set(train_time_19[:,0,0].astype(np.int64)))
    #valid_loc_index = list(set(valid_time_2_19[:,0,0].astype(np.int64)))
    valid_loc_index = list(set(valid_time_1[:,0,0].astype(np.int64)))
    #test_loc_index = list(set(test_time_2_19[:,0,0].astype(np.int64)))
    test_loc_index = list(set(test_time_1[:,0,0].astype(np.int64)))

    #scaling - time # 지역별 스케일링
    #scaling - time # 지역별 스케일링
    train_time = time_data[loc_index[ : 69], : 201, 2:] ## 전부 샘플링

    if scaler == None:
        scaler, _ = scaleing_time(train_time)
        _, train_time_19[:,:,2:] = scaleing_time(train_time_19[:,:,2:], scaler) # 2이 flow_pop
        _, train_time_20[:,:,2:] = scaleing_time(train_time_20[:,:,2:], scaler)
    else:
        _, train_time_19[:,:,2:] = scaleing_time(train_time_19[:,:,2:], scaler)
        _, train_time_20[:,:,2:] = scaleing_time(train_time_20[:,:,2:], scaler)


    _, valid_time_1[:,:,2:] = scaleing_time(valid_time_1[:,:,2:], scaler)
    #_, valid_time_2_19[:,:,2:] = scaleing_time(valid_time_2_19[:,:,2:], time_scaler)
    #_, valid_time_2_20[:,:,2:] = scaleing_time(valid_time_2_20[:,:,2:], time_scaler)
    _, test_time_1[:,:,2:] = scaleing_time(test_time_1[:,:,2:], scaler)
    #_, test_time_2_19[:,:,2:] = scaleing_time(test_time_2_19[:,:,2:], time_scaler)
    #_, test_time_2_20[:,:,2:] = scaleing_time(test_time_2_20[:,:,2:], time_scaler)

    train_valid_test = [train_time_19, train_time_20,
                         valid_time_1,
                         # valid_time_2_19, valid_time_2_20,
                          test_time_1]
                          # test_time_2_19, test_time_2_20]
    train_valid_test_index = [train_loc_index, valid_loc_index, test_loc_index]

    return train_valid_test, train_valid_test_index, scaler

def split_notime_data(nontime_data, train_valid_test_index):
    train_loc_index, valid_loc_index, test_loc_index= train_valid_test_index
    # make no time data
    nontime_data = nontime_data[['HDONG_CD', 'time', 'tot_pop', 'age_80U', 'AREA']]
    nontime_data = nontime_data.groupby(['HDONG_CD']).sum()

    # split no time data
    train_no_time = nontime_data.loc[train_loc_index]
    valid_no_time = nontime_data.loc[valid_loc_index]
    test_no_time = nontime_data.loc[test_loc_index]

    # scaleing no time data*
    no_time_scaler, train_no_time = scaleing_no_time(train_no_time)
    _,              valid_no_time = scaleing_no_time(valid_no_time)
    _,              test_no_time  = scaleing_no_time(test_no_time)

    notime = [train_no_time, valid_no_time, test_no_time]
    #return pd.concat(notime), no_time_scaler
    return train_no_time, no_time_scaler

def split_sequence(sequence, input_window = INPUT_WINDOW, output_window = OUTPUT_WINDOW, target_index  = 2):
    x, y = list(), list()
    #print(sequence.shape)
    for day in range(ROLLSIZE, sequence.shape[0]):
        start_idx = day
        cut_idx = day + input_window
        end_idx = day + input_window + output_window

        if end_idx + ROLLSIZE > (len(sequence)):#
            break
        # input_seires (x)
        ###
        seq_x = sequence[start_idx  - ROLLSIZE : cut_idx, :]
        trend_cycle_x = append_trend_cycle(seq_x[:,target_index])
        seq_x = np.concatenate([seq_x[ROLLSIZE:,3:],
                                trend_cycle_x[ROLLSIZE:]], axis = 1)
        ###
        seq_y = sequence[cut_idx - ROLLSIZE : end_idx  + ROLLSIZE, :]
        trend_cycle_y = append_trend_cycle(seq_y[:,target_index])
        seq_y = trend_cycle_y[ROLLSIZE:-ROLLSIZE,:]
        ###
        x.append(seq_x)
        y.append(seq_y)

    return np.array(x), np.array(y)

def seq2cycle(seq):
    return pd.Series(seq).rolling(ROLLSIZE).mean()


def seq2cycle_knn(seq):
    trend_x = np.zeros(seq.shape)
    ROLLING = 3
    for idx in range(ROLLING, (len(seq)-1) + (-ROLLING+1)):
        trend_x[idx] =  seq[idx- ROLLING : idx- ROLLING + 6 + 1].mean()

    idx = (len(seq)-1) +(-ROLLING+1)
    trend_x[idx] =  seq[idx- ROLLING-1 : idx- ROLLING + 5 + 1].mean()
        
    idx = (len(seq)-1) +(-ROLLING+2)
    trend_x[idx] =  seq[idx- ROLLING-2 : idx- ROLLING + 4 + 1].mean()  

    idx = (len(seq)-1)+(-ROLLING + 3)
    trend_x[idx] =  seq[idx- ROLLING-3 : idx- ROLLING + 3 + 1].mean()
    return trend_x

def seq2cycle_weight(seq):
    trend_x = np.zeros(seq.shape)
    ROLLING = 3
    for idx in range(ROLLING, (len(seq)-1) + (-ROLLING+1)):
        trend_x[idx] =  seq[idx- ROLLING]*1 +\
                        seq[idx- ROLLING + 1]*2 +\
                        seq[idx- ROLLING + 2]*3 +\
                        seq[idx- ROLLING + 3]*4 +\
                        seq[idx- ROLLING + 4]*3 +\
                        seq[idx- ROLLING + 5]*2 +\
                        seq[idx- ROLLING + 6]*1
        trend_x[idx] =  trend_x[idx] / 16

    idx = (len(seq)-1) +(-ROLLING+1)
    trend_x[idx] =  seq[idx- ROLLING]*1 +\
                        seq[idx- ROLLING + 1]*2 +\
                        seq[idx- ROLLING + 2]*4 +\
                        seq[idx- ROLLING + 3]*4 +\
                        seq[idx- ROLLING + 4]*3 +\
                        seq[idx- ROLLING + 5]*2 
    trend_x[idx] =  trend_x[idx] / 16
        
    idx = (len(seq)-1) +(-ROLLING+2)
    trend_x[idx] =  seq[idx- ROLLING]*1 +\
                        seq[idx- ROLLING + 1]*4 +\
                        seq[idx- ROLLING + 2]*4 +\
                        seq[idx- ROLLING + 3]*4 +\
                        seq[idx- ROLLING + 4]*3
    trend_x[idx] =  trend_x[idx] / 16    

    idx = (len(seq)-1)+(-ROLLING + 3)
    trend_x[idx] =  seq[idx- ROLLING]*4 +\
                        seq[idx- ROLLING + 1]*4 +\
                        seq[idx- ROLLING + 2]*4 +\
                        seq[idx- ROLLING + 3]*4
    trend_x[idx] =  trend_x[idx] / 16
    return trend_x



def append_trend_cycle(flow_pop):
    new_flow_pop = np.zeros([len(flow_pop), 2])
    #new_flow_pop[:, 0] = seq2cycle(flow_pop)[ROLLSIZE:]
    new_flow_pop[:, 0] = seq2cycle_weight(flow_pop)
    new_flow_pop[:, 1] = flow_pop - new_flow_pop[:, 0]
    #print(new_flow_pop)
    return new_flow_pop

def make_data(data_list, notime):
    x_time_batch = []
    x_notime_batch = []
    y_time_batch = []

    if len(data_list) != 2:
        data_list = [data_list]

    for data in data_list:
        x_time, x_notime, y_time = make_time_notime_data(data, notime)
        x_time_batch.append(x_time)
        x_notime_batch.append(x_notime)
        y_time_batch.append(y_time)
        #print(x_time.shape)

    x_time_batch = np.concatenate(x_time_batch)
    x_notime_batch = np.concatenate(x_notime_batch)
    y_time_batch = np.concatenate(y_time_batch)

    print(x_time_batch.shape)
    print(x_notime_batch.shape)
    print(y_time_batch.shape)
    
    return x_time_batch, x_notime_batch, y_time_batch

def make_time_notime_data(time_data, notime_data, input_window = INPUT_WINDOW, out_window = OUTPUT_WINDOW):    

    x_time = []
    x_notime = []
    y_time = []
    for loc in range(time_data.shape[0]):
        loc_code = time_data[loc,0,0]
        #print(time_data[loc_code,time_idx,0,0])
        x, y = split_sequence(time_data[loc,:,:], input_window, out_window)
        notime = notime_data.loc[loc_code]
        aug_notime = np.zeros([x.shape[0], notime.shape[0]])
        aug_notime[:,:] = notime
        x_time.append(x)
        x_notime.append(aug_notime)
        y_time.append(y)
    
    x_time = np.concatenate(x_time)
    x_notime = np.concatenate(x_notime)
    y_time = np.concatenate(y_time)
    
    #print(x_time.shape)
    #print(x_notime.shape)
    #print(y_time.shape)
    
    return x_time.astype(np.float64), x_notime.astype(np.float64), y_time.astype(np.float64)


def numpy2tensor(data_list):
    '''
        [train_time, train_notime, train_y,
         valid_time, valid_notime, valid_y,
         test_time, test_notime, test_y]
    '''
    tensor_list = []
    for data in data_list:
        tensor_list.append(torch.FloatTensor(data).to(device))
    return tensor_list

def append_cycle_size(data_list):
    for tr_va_te in [0,3,6]:
        cycle_size = data_list[0+tr_va_te][:,:,-1].max(axis = 1) - data_list[0+tr_va_te][:,:,-1].min(axis = 1)
        data_list[1+tr_va_te] = np.concatenate([data_list[1+tr_va_te], cycle_size.reshape(-1,1)], axis = 1)
    return data_list

def append_trend_size(data_list):
    for tr_va_te in [0,3,6]:
        cycle_size = data_list[0+tr_va_te][:,:,-2].max(axis = 1) - data_list[0+tr_va_te][:,:,-2].min(axis = 1)
        data_list[1+tr_va_te] = np.concatenate([data_list[1+tr_va_te], cycle_size.reshape(-1,1)], axis = 1)
    return data_list


def tensor2numpy(data_list):
    '''
        [train_time, train_notime, train_y,
         valid_time, valid_notime, valid_y,
         test_time, test_notime, test_y]
    '''
    numpy_list = []
    for data in data_list:
        numpy_list.append(data.cpu().detach().numpy())
    return numpy_list

