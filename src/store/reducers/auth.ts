import { createSlice } from '@reduxjs/toolkit';
import type { PayloadAction } from '@reduxjs/toolkit';

interface AuthInterface {
  user: any;
  isLogin: boolean;
}

export const initialState: AuthInterface = {
  user: null,
  isLogin: false,
};

export const authSlice = createSlice({
  name: 'auth',
  initialState,
  reducers: {
    userAuth: (state, action: PayloadAction<any>) => {
      state.user = action.payload;
      state.isLogin = true;
    },
  },
});

// Action creators are generated for each case reducer function
export const { userAuth } = authSlice.actions;

export default authSlice.reducer;
