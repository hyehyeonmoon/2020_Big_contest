import numpy as np
import warnings
warnings.filterwarnings(action='ignore')
import torch
import torch.nn as nn
import torch.nn.functional as F
import torchvision
import torchvision.transforms as transforms
from torch.utils.data import Dataset,DataLoader
import torch.optim as optim
torch.manual_seed(1015)
# define 'device' to upload tensor in gpu
device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")


class LSTM_FC_trend(nn.Module):
    def __init__(self, input_size, hidden_size, no_time_size):
        # time model
        super(LSTM_FC_trend, self).__init__()
        self.hidden_size = hidden_size
        self.lstm = nn.LSTM(input_size, hidden_size, batch_first=True)
        self.time_fc = nn.Linear(hidden_size, 1)
        
        # no time model
        self.no_time_fc = nn.Sequential(
            nn.Linear(no_time_size,4),
            nn.ReLU(inplace=True),
            nn.Linear(4,4),
            nn.ReLU(inplace=True),
            nn.Linear(4,4)
        )
        # merge model
        self.merge_fc = nn.Sequential(
            nn.Linear(11, 7),
            nn.ReLU(inplace=True),
            nn.Linear(7, 7),
            nn.Sigmoid()
        )
    
    def forward(self, x_time, x_no_time):
        # time part
        x = torch.tensor(x_time)
        mini_trend = torch.min((x_time[:,:,-2]),dim = 1)[0]
        mini_trend = mini_trend.view(-1,1)
        maxi_trend = torch.max((x_time[:,:,-2]), dim = 1)[0]
        maxi_trend = maxi_trend.view(-1,1)

        out_time, _ = self.lstm(x_time)
        out_time = self.time_fc(out_time[:, -7:, :])#
        
        # no_time part
        out_no_time = self.no_time_fc(x_no_time)
        #print(out_no_time.shape)
        out = torch.cat((out_time.view(-1,7), out_no_time), 1)
        out = self.merge_fc(out)
        out = out *(maxi_trend - mini_trend) + mini_trend
        return out