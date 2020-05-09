clc
clear

filename = 'Football_Q3Clean.csv'

F = readtable(filename);

%%

rows = F.Q1 == 1
TempT = F(rows,:) % subset in R
Q3B_Average(1,1) = mean(TempT.Q3B)


%%

for i = 1:10 % (i in 1:10){
    rows = (F.Q1 == i) | (F.Q1A == i) | (F.Q1B == i) | (F.Q1C == i);
    TempT = F(rows,:); % subset in R
    NumPeople(i,1) = size(TempT,1);
end


%%
for i = 1:10 % (i in 1:10){
    rows = (F.Q1 == i) | (F.Q1A == i) | (F.Q1B == i) | (F.Q1C == i);
    TempT = F(rows,:); % subset in R
    Q3B_Average(i,1) = mean(TempT.Q3B(~isnan(TempT.Q3B)));
    Q3B_Average(i,1:9) = mean(TempT{:,9:17});
end

% add in last row, the total average of the Table
Q3B_Average(11,:) = mean(F{:,9:17})


%% More than one category....


rows = (F.Q1 == 1) | (F.Q1A == 1) | (F.Q1B == 1) | (F.Q1C == 1)
TempT = F(rows,:) % subset in R
Q3B_Average(1,1) = mean(TempT.Q3B)



%%
close all % pics go away...

bar(Q3B_Average(1:10,1))


bar(Q3B_Average(1:10,1),'basevalue',Q3B_Average(11,1));
title('The Parternship is a Good Fit','FontSize',32)
ylabel('Level of Agreement')

% mini function
% col = 2;
% Q3Labels = {'The Parternship is a Good Fit',...
%     'a', ...
%     'excel',...
%     'tradition'...
%     'att'}; %manually entered...
% 
% 
% 
% bar(Q3A_Average(1:10,col),'basevalue',Q3A_Average(11,col));
% title(Q3Labels{col},'FontSize',32)
% ylabel('stuff')
