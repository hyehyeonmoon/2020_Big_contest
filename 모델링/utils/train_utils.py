
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np


import torch
import torch.nn as nn
import torch.nn.functional as F
import torchvision
import torchvision.transforms as transforms
from torch.utils.data import Dataset,DataLoader
import torch.optim as optim
torch.manual_seed(1015)
import random
from utils.preprocess_utils import *

from matplotlib import font_manager, rc
font_name = font_manager.FontProperties(fname="c:/Windows/Fonts/malgun.ttf").get_name()
rc('font', family=font_name)
import matplotlib
matplotlib.rcParams['axes.unicode_minus'] = False
# define 'device' to upload tensor in gpu
#device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")

def plot_hist(train_error, valid_error, hist, ylim = 0.02):
    plt.figure(figsize = [10, 5])
    plt.plot(train_error, label = 'train_mse')
    plt.plot(valid_error, label = 'valid_mse')

    plt.axvline(hist['best_val_epoch'],
                color = 'red', ls = 'dashed', label = f"best_val_epoch : {hist['best_val_epoch']} epochs")
    plt.axhline(hist['best_val_error'],
                color = 'red', ls = 'dashed', label = f"best_val_mse : {hist['best_val_error']:1.5f}")

    plt.xticks(label = 'epochs')
    plt.legend()
    plt.ylim(0,ylim)
    plt.show()

def plot_models_valid_mse(model_valid_scores):
    plt.figure(figsize=(10, 4))
    splot=sns.barplot(x="model", y="validation_MSE", data=model_valid_scores, color = '#5A87B9')
    plt.ylabel("validation_MSE",size=14)
    plt.xlabel("model",size=14)
    plt.xticks(fontsize = 13)
    plt.yticks(fontsize = 13)
    plt.title("6가지 모델의 검증 데이터 성능 비교", size=18)
    for p in splot.patches:
        splot.annotate(format(p.get_height(), '.4f'), 
                       (p.get_x() + p.get_width() / 2., p.get_height()), 
                       ha = 'center', va = 'center', 
                       color = "white",
                       size=15,
                       xytext = (0, -12), 
                       textcoords = 'offset points')
    plt.show()


def plot_predict(train_x, train_y, train_pred,
                    valid_x, valid_y, valid_pred, plot_numbers = 10):
    for i in range(plot_numbers):

        i = random.randint(1,len(train_x)-1)
        plt.figure(figsize = (15, 3))
        
        plt.subplot(1,2,1)
        
        plt.plot(np.arange(21), train_x[i,:],   # m_train_time.cpu().detach().numpy()[i,:,-2],
                marker = 'o', color = 'black', label = '입력 유동인구')# 검정
        plt.plot(np.arange(21,28), train_y[i],    #m_train_y[:,:,0].cpu().detach().numpy()[i],
                marker = 'o', color = '#BD434D', label = '실제 유동인구') #빨강
        plt.plot(np.arange(21,28),train_pred[i],    #.cpu().detach().numpy()[i],
                color = '#5A87B9', label = '예측 유동인구', marker = 'x', ls = '--') # 파랑
        start_day = train_idx2day(i,  len(train_x))
        for vline in range(4):
            plt.axvline(vline* 7 , color = '#E7825C', ls = '--')
        plt.xticks(ticks = np.arange(28), labels = make_xticks(start_day))
        plt.title(f'{idx2dong(i, len(train_x))} (훈련[Train]))')
        plt.legend(loc='lower left')

        i = random.randint(1,len(valid_x)-1)
        plt.subplot(1,2,2)
        plt.plot(np.arange(21), valid_x[i,:],
                marker = 'o', color = 'black', label = '입력 유동인구')
        plt.plot(np.arange(21,28), valid_y[i],
                marker = 'o', color = '#BD434D', label = '실제 유동인구')
        plt.plot(np.arange(21,28), valid_pred[i],
                color = '#5A87B9', label = '예측 유동인구', marker = 'x', ls = '--')
        for vline in range(4):
            plt.axvline(vline* 7 , color = '#E7825C', ls = '--')

        start_day = valid_idx2day(i,  len(valid_x))
        plt.xticks(ticks = np.arange(28), labels = make_xticks(start_day))
        plt.title(f'{idx2dong(i, len(valid_x))} (검증[Validation])')

        plt.legend(loc='lower left')
        plt.show()


def plot_test_predict(test_x, test_y, test_pred):
    plt.figure(figsize = (15, 7))
    for i in range(4):
        plt.subplot(2,2,i+1)
        i = random.randint(1,len(test_x)-1)
        plt.plot(np.arange(21), test_x[i,:],   # m_train_time.cpu().detach().numpy()[i,:,-2],
                marker = 'o', color = 'black', label = '입력 유동인구')
        plt.plot(np.arange(21,28), test_y[i],    #m_train_y[:,:,0].cpu().detach().numpy()[i],
                marker = 'o', color = '#BD434D', label = '실제 유동인구')
        plt.plot(np.arange(21,28),test_pred[i],    #.cpu().detach().numpy()[i],
                color = '#5A87B9', label = '예측 유동인구', marker = 'x', ls = '--')
        for vline in range(4):
            plt.axvline(vline* 7 , color = '#E7825C', ls = '--')
        
        start_day = test_idx2day(i,  len(test_x))
        plt.xticks(ticks = np.arange(28), labels = make_xticks(start_day))
        plt.title(f'{idx2dong(i, len(test_x))} (평가[Test])')
        plt.legend(loc='lower left')
    plt.show()

try:
    time = pd.read_csv('../data/original/time_data.txt', sep = ' ')
    nontime = pd.read_csv('../data/original/nontime_data.txt', sep = ' ')
except:
    time = pd.read_csv('data/original/time_data.txt', sep = ' ')
    nontime = pd.read_csv('data/original/nontime_data.txt', sep = ' ')
    
day_list = time.STD_YMD.unique()
def train_idx2day(idx, train_len):
    day_len = int(train_len//69)
    day_idx = idx % day_len
    if day_idx <= 86:
        return day_list[day_idx + 3]
    elif day_idx > 86:
        return day_list[day_idx + 36]
        
def valid_idx2day(idx, vaild_len):
    day_len = int(vaild_len//69)
    day_idx = idx % day_len
    return day_list[day_idx + 171]

def test_idx2day(idx, test_len):
    day_len = int(test_len//69)
    day_idx = idx % day_len
    return day_list[day_idx + 191]

code_list = time.HDONG_CD.unique()
code2name = time.groupby('HDONG_CD')['HDONG_NM'].apply(lambda x :list(x)[0])

def idx2dong(idx, total_len):
    day_size = int(total_len/69)
    idx = int(idx//day_size)
    code = code_list[idx]
    dong_name = code2name[code]
    return dong_name

def make_xticks(start_day):
    start_idx = int(np.argwhere(day_list == start_day))
    xticks = day_list[start_idx: start_idx + 28].copy()
    for i in range(len(xticks)):
        if i%7 != 0:
            xticks[i] = None
    return xticks

def add_datas(data_list1, data_list2):
    new_data = []
    for i in range(len(data_list1)):
        new_data.append(data_list1[i] + data_list2[i])
    return new_data


def rescale_data(data_list, scaler):
    new_data_list = []
    for data in data_list:
        shape = data.shape
        data = scaler.inverse_transform(data.reshape(-1), select_col = 0)
        new_data_list.append(data.reshape(shape))
    return new_data_list