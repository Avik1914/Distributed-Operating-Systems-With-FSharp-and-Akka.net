class Solution {
    public int findPaths(int m, int n, int N, int i, int j) {
        long dp=new Long[m*n][N+1];
        int[][] dir={{-1,0},{0,-1},{1,0},{0,1}};
        
        for(int i=1;i<=N;i++){
            for(int j=0;j<)
        }
        
        
        return (int)dfs(m,n,i,j,N);
    }
    
    public long dfs123442(int m,int n,int i,int j,int N){
        if(N<0)
            return 0;
        if(i<0 || j<0 || i>=m || j>=n)
            return 1;
        
        if(dp[n*i+j][N]!=null)
            return dp[n*i+j][N];
       
        dp[n*i+j][N]=dfs(m,n,i+1,j,N-1)+dfs(m,n,i,j+1,N-1)
                +dfs(m,n,i-1,j,N-1)+dfs(m,n,i,j-1,N-1);
        
        dp[n*i+j][N]=dp[n*i+j][N]%1000000007;
        
        return dp[n*i+j][N];
    }
}