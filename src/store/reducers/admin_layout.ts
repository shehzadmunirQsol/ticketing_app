import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { RootState } from '~/store/store';

interface languageInterface {
  id: number;
  code: 'en' | 'ar';
}

// Define a type for the slice state
interface LayoutState {
  isSidebarOpen: boolean;
  language: languageInterface;
}

// Define the initial state using that type
const initialState: LayoutState = {
  isSidebarOpen: true,
  language: {
    id: 1,
    code: 'en',
  },
};

export const adminLayoutSlice = createSlice({
  name: 'layout',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    toggleSidebar: (state) => {
      state.isSidebarOpen = !state.isSidebarOpen;
    },
    switchLanguage: (state, action: PayloadAction<languageInterface>) => {
      state.language = action.payload;
    },
  },
});

export const { toggleSidebar, switchLanguage } = adminLayoutSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectCount = (state: RootState) => state.layout;

export default adminLayoutSlice.reducer;
