import { PayloadAction, createSlice } from '@reduxjs/toolkit';
import { RootState } from '~/store/store';

// Define a type for the slice state
interface LayoutState {
  isSidebarOpen: boolean;
  selectedEvent: any;
}

// Define the initial state using that type
const initialState: LayoutState = {
  isSidebarOpen: true,
  selectedEvent: null,
};

export const adminLayoutSlice = createSlice({
  name: 'layout',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    toggleSidebar: (state) => {
      state.isSidebarOpen = !state.isSidebarOpen;
    },
    setSelectedEvent: (state, action: PayloadAction<any>) => {
      state.selectedEvent = action.payload;
    },
  },
});

export const { toggleSidebar, setSelectedEvent } = adminLayoutSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectCount = (state: RootState) => state.layout;

export default adminLayoutSlice.reducer;
