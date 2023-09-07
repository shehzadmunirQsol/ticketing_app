import { createSlice } from '@reduxjs/toolkit';
import type { PayloadAction } from '@reduxjs/toolkit';
import { RootState } from '~/store/store';

interface Lang {
  dir: 'rtl' | 'ltr';
  lang: 'en' | 'ar';
  lang_id: 1 | 2;
}
// Define a type for the slice state
interface LayoutState {
  theme: string;
  lang: Lang;
}

// Define the initial state using that type
const initialState: LayoutState = {
  theme: 'dark',
  lang: { dir: 'ltr', lang: 'en', lang_id: 1 },
};

export const layoutSlice = createSlice({
  name: 'layout',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    setTheme: (state, action: PayloadAction<string>) => {
      state.theme = action.payload;
    },
    toggleLang: (state, action: PayloadAction<Lang>) => {
      console.log('action.payload', action.payload);
      state.lang = action.payload;
    },
  },
});

export const { setTheme, toggleLang } = layoutSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectCount = (state: RootState) => state.layout;

export default layoutSlice.reducer;
