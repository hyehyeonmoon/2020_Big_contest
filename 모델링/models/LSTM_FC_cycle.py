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

class LSTM_FC_cycle(nn.Module):
    def __init__(self, input_size, hidden_size, no_time_size):

        # time model
        super(LSTM_FC_cycle, self).__init__()
        self.hidden_size = hidden_size
        self.lstm = nn.LSTM(input_size, hidden_size, batch_first=True)
        
        self.lstm_week1 = nn.LSTM(input_size, hidden_size, batch_first=True)
        self.lstm_week2 = nn.LSTM(input_size, hidden_size, batch_first=True)
        self.lstm_week3 = nn.LSTM(input_size, hidden_size, batch_first=True)

        self.time_fc_gru = nn.Sequential(
            nn.Linear(hidden_size, 1)
        )
        self.time_fc_week1 = nn.Sequential(
            nn.Linear(hidden_size, 1)
        )
        self.time_fc_week2 = nn.Sequential(
            nn.Linear(hidden_size, 1)
        )
        self.time_fc_week3 = nn.Sequential(
            nn.Linear(hidden_size, 1)
        )

        self.week_merge = nn.Sequential(
            nn.Linear(3, 1)
        )

        self.dropout = nn.Dropout(p=0.2)
        # no time model
        self.no_time_fc = nn.Sequential(
            nn.Linear(no_time_size, 4),
            nn.ReLU(inplace=True),
            nn.Linear(4,4)
            #nn.ReLU(inplace=True)
        )

        # merge model
        self.merge_fc = nn.Sequential(
            nn.Linear(11, 11),
            nn.ReLU(inplace=True),
            nn.Linear(11, 7)

        )

    def forward(self, x_time, x_no_time):
        #print(x_time[:,:,-1].shape)
        # time part
        # x_time의 12개 채널 정보
        # 'card_use', 'holiday', 'day_corona',
        # 'ondo', 'subdo', 'rain_snow',
        # 'dayofyear_sin', 'dayofyear_cos', 'weekday_sin', 'weekday_cos',
        # 'flow_trend', flow_cycle'
        # time part
        x = torch.tensor(x_time)
        mini_cycle = torch.min((x_time[:,:,-1]),dim = 1)[0]
        mini_cycle = mini_cycle.view(-1,1)
        maxi_cycle = torch.max((x_time[:,:,-1]), dim = 1)[0]
        maxi_cycle = maxi_cycle.view(-1,1)
        
        x[:,:,-1] = (x[:,:,-1] - mini_cycle)/(maxi_cycle-mini_cycle)
        x[:,:,-1] = (x[:,:,-1] - mini_cycle)/(maxi_cycle-mini_cycle)

        out_time_lstm_week1, _ = self.lstm(x[:,   :7 , :])
        out_time_lstm_week2, _ = self.lstm(x[:, 7 :14, :])
        out_time_lstm_week3, _ = self.lstm(x[:, 14:  , :])

        #out_time의 shape : [batch, input_window_size, hidden_size]
        out_time_lstm_week1 = self.time_fc_week1(out_time_lstm_week1)
        out_time_lstm_week2 = self.time_fc_week2(out_time_lstm_week2)
        out_time_lstm_week3 = self.time_fc_week3(out_time_lstm_week3)

        out_time = self.week_merge(torch.cat((out_time_lstm_week1.view(-1,7,1),
                                                 out_time_lstm_week2.view(-1,7,1), 
                                                 out_time_lstm_week3.view(-1,7,1)), 2))
        out_time = out_time.view(-1,7)
        # no_time part
        out_no_time = self.no_time_fc(x_no_time).view(-1,4)

        out = torch.cat((out_time,out_no_time),1)    
        out = self.merge_fc(out)

        out = out *(maxi_cycle - mini_cycle) + mini_cycle
        return out
