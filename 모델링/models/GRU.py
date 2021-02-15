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

class GRU(nn.Module):
    def __init__(self, input_size, hidden_size, no_time_size):
        # time model
        super(GRU, self).__init__()
        self.hidden_size = hidden_size
        self.lstm = nn.GRU(input_size, hidden_size, batch_first=True)
        self.time_fc = nn.Linear(hidden_size, 7)
    def forward(self, x_time, x_no_time):
        # time part
        out_time, _ = self.lstm(x_time)
        out_time = self.time_fc(out_time[:, -1, :])#
        return out_time.view(-1,7)