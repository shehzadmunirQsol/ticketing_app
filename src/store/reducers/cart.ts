import { createSlice } from '@reduxjs/toolkit';
import type { PayloadAction } from '@reduxjs/toolkit';
import { RootState } from '~/store/store';

interface Cart {
  id: number;
  customer_id: number;
}

interface CartItemInterface {
  subscription_type?: 'weekly' | 'monthly' | 'quarterly';
  cart_id: number;
  event_id: number;
  quantity: number;
  is_subscribe: boolean;
}
// Define a type for the slice state
interface CartState {
  cart: (Cart & { CartItems: CartItemInterface[] }) | null;
}

// Define the initial state using that type
const initialState: CartState = {
  cart: null,
};

export const cartSlice = createSlice({
  name: 'cart',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    addCart: (state, action: PayloadAction<CartState>) => {
      state.cart = action.payload.cart;
    },
  },
});

export const { addCart } = cartSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectCount = (state: RootState) => state.cart;

export default cartSlice.reducer;
