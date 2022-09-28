function [index]=ntoi(value,grid);

% August/25

% Procedure to convert numbers to the correspondent indexes in the grid

n=max(size(grid));
step=(grid(n)-grid(1))/(n-1);
[m,m0]=size(value);
index=round((value-grid(1)*ones(m,m0))/step)+ones(m,m0);
index=max(min(index,n),1);