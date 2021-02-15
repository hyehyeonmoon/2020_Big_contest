# fp-ts
1. data/origianl/ 경로에 time_data, nontime_data 생성
2.  preprocess.py 를 실행하여 모델 훈련을 위한 전처리 데이터를 생성한다. (6월 예측에 사용되는 데이터도 생성)
3. train_morning 폴더 속morning_train_trend.ipynb, morning_train_cycle.ipynb , morning_total_fp.ipynb            파일을 차례대로 실행한다. ( #순서 주의# trend와 cycle이 생성된 후 total 유동인구 생성가능)
4. 마찬가지로 train_lunch 폴더, train_evening 폴더를 실행한다.

## [Linux Tree]

fp-ts/  
│  
├── data/  #.gitignore       
│   │      
│   ├── original/    # 시계열 변수와 비 시계열 변수가 들어있는 폴더   
│   │        ├── nontime_data.txt  
│   │        └── time_data.txt       
│   │   
│   ├── predict_june/    # 완성된 모델로 6월의 예측을 위한 데이터를 보관하는 폴더  
│   │   ├── predict_cycle_trend/ #완성된 모델의 6월 예측 결과   
│   │   │    ├── june_morning_trend_pred.plk  
│   │   │    ├── june_morning_cycle_pred.plk    
│   │   │    ├── june_lunch_trend_pred.plk           
│   │   │    ├── june_lunch_cycle_pred.plk  
│   │   │    ├── june_evening_trend_pred.plk    
│   │   │    └──  june_evening_cycle_pred.plk     
│   │   │   
│   │   ├── preprosess_june/ # 6월 예측을 위해 생성한 파일 preprocess.py를 통해 생성       
│   │   │    ├── morning_june_time.plk  
│   │   │    ├── morning_june_notime.plk    
│   │   │    ├── lunch_june_time.plk  
│   │   │    ├── lunch_june_notime.plk    
│   │   │    ├── evening_june_time.plk  
│   │   │    └──evening_june_notime.plk    
│   │   │    
│   │   └── result_june/ #6월 예측결과를 통해 만든 결과         
│   │        ├── evening_inother.csv# 다른 동에 비해 유동인구가 붐비는 정도 1~5  
│   │        ├──  evening_insame.csv# 같은지역 근 한 달 동안에 비해 유동인구가 붐비는 정도 1~5  
│   │        ├──  lunch_inother.csv  
│   │        ├── lunch_insame.csv  
│   │        ├──  morning_inother.csv  
│   │        └──  morning_insame.csv    
│   │  
│   ├── train_results/  # 모델 훈련 결과    
│   │        ├── evening_results  # 저녁 데이터의 trend, cycle결과    
│   │ 	   │     ├──  e_cycle_results_list.pkl  
│   │        │     ├──  e_cycle_results_test.pkl  
│   │   	 │     ├──  e_total_results_list.pkl #결합 후 최종 결과 
│   │   	 │     ├──  e_total_results_test.pkl# test데이터 예측 결과  
│   │    	│     ├──  e_trend_results_list.pkl  
│   │        │     └──  e_trend_results_test.pkl     
│   │        │    
│   │        ├── lunch_results        
│   │ 	   │     ├──  l_cycle_results_list.pkl  
│   │        │     ├──  l_cycle_results_test.pkl  
│   │   	 │     ├──  l_total_results_list.pkl  
│   │   	 │     ├──  l_total_results_test.pkl  
│   │    	│     ├──  l_trend_results_list.pkl  
│   │        │     └──  l_trend_results_test.pkl     
│   │        │      
│   │        └── morning_results     
│   │ 	          ├──  m_cycle_results_list.pkl  
│   │               ├──  m_cycle_results_test.pkl  
│   │   	        ├──  m_total_results_list.pkl  
│   │   	        ├──  m_total_results_testpkl  
│   │    	       ├──  m_trend_results_list.pkl  
│   │               └──  m_trend_results_test.pkl     
│   │                     
│   └── preprocess/ # 모델 학습을 위해 전처리 한 결과 데이터, preprocess를 통해 생성  
│             ├── m_data_list.plk  
│             ├── l_data_list.plk    
│             ├── e_data_list.plk           
│             ├── l_data_list.plk  
│             └── scalers.plk    
│          
│  
├── models/# 학습에 사용된 모델  
│     ├── LSTM.py  
│     ├── GRU.py     
│     ├── LSTM_FC_trend.py  
│     └── LSTM_FC_cycle.py  
│  
├── preprocess.py # 학습을 위한 데이터 전처리 폴더  
│      
├── train_morning/     
│     ├── best_model/       
│      │        └── # 훈련된 6가지 모델의 결과.pkl   
│      ├── morning_train_trend.ipynb     
│      ├── morning_train_cycle.ipynb     
│      └── morning_total_fp.ipynb     
│    

├── train_lunch/     
│     ├── best_model/       
│      │        └── # 훈련된 6가지 모델의 결과.pkl   
│      ├── lunch_train_trend.ipynb     
│      ├── lunch_train_cycle.ipynb      
│      └── lunch_train_total_fp.ipynb    
│ 
├── train_evening/     
│     ├── best_model/       
│      │        └── # 훈련된 6가지 모델의 결과.pkl   
│     ├── evening_train_trend.ipynb  
│     ├── evening_train_cycle.ipynb      
│     └── evening_total_fp.ipynb   
│        
├── predict_flow.zip  # 9개의 ipython파일의 출력을 html타입으로 저장   
└── README.md   