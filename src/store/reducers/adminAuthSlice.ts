import { createSlice } from '@reduxjs/toolkit';
import type { PayloadAction } from '@reduxjs/toolkit';

export interface AdminUserInterface {
    id?: number,
    email?: string,
    password?: string,
    name?: string,
    role_id?: number,
    is_deleted?: boolean,
    created_at?: Date,
    updated_at?: Date,
}

interface AdminAuthInterface {
    user: AdminUserInterface | null;
    isLogin: boolean;
}

export const initialState: AdminAuthInterface = {
    user: null,
    isLogin: false
};

export const adminAuthSlice = createSlice({
    name: 'userAdminAuth',
    initialState,
    reducers: {
        userAdminAuth: (state, action: PayloadAction<AdminUserInterface | null>) => {
            state.user = action.payload;
        },
        userAdminIsLogin: (state, action: PayloadAction<boolean>) => {
            state.isLogin = action.payload;
        }
    },
});

// Action creators are generated for each case reducer function
export const { userAdminAuth, userAdminIsLogin } = adminAuthSlice.actions;

export default adminAuthSlice.reducer;
