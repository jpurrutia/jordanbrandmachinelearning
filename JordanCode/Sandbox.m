clc
clear

filename = 'Combined_small.csv'

T = readtable(filename);

%%

% rows = T.Q1 == 1
% TempT = T(rows,:) % subset in R
% Q3A_Average(1,1) = mean(TempT.Q3A)


%%
for i = 1:10 % (i in 1:10){
    rows = T.Q1 == i;
    TempT = T(rows,:); % subset in R
%     Q3A_Average(i,1) = mean(TempT.Q3A);
    Q3A_Average(i,1:10) = mean(TempT{:,8:17});
end

% add in last row, the total average of the Table
Q3A_Average(11,:) = mean(T{:,8:17});


%% More than one category....


rows = (T.Q1 == 1) | (T.Q1a == 1)
TempT = T(rows,:) % subset in R
Q3A_Average(1,1) = mean(TempT.Q3A)







%%
close all % pics go away...

bar(Q3A_Average(1:10,1))


% bar(Q3A_Average(1:10,1),'basevalue',Q3A_Average(11,1));
% title('The Parternship is a Good Fit','FontSize',32)
% ylabel('stuff')

% mini function
col = 2;
Q3Labels = {'The Parternship is a Good Fit',...
    'a', ...
    'excel',...
    'tradition'...
    'att'}; %manually entered...



bar(Q3A_Average(1:10,col),'basevalue',Q3A_Average(11,col));
title(Q3Labels{col},'FontSize',32)
ylabel('stuff')














