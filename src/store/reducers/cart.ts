import { createSlice } from '@reduxjs/toolkit';
import type { PayloadAction } from '@reduxjs/toolkit';
import { RootState } from '~/store/store';

interface Cart {
  id: number | null;
  customer_id: number | null;
}

interface CartItemInterface {
  id: number;
  subscription_type: 'weekly' | 'monthly' | 'quarterly' | null;
  cart_id: number;
  event_id: number;
  quantity: number;
  is_subscribe: boolean;
}
// Define a type for the slice state
interface CartState {
  cart: Cart & { cartItems: CartItemInterface[] };
}

// Define the initial state using that type
const initialState: CartState = {
  cart: {
    id: null,
    customer_id: null,
    cartItems: [],
  },
};

export const cartSlice = createSlice({
  name: 'cart',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    addCart: (state, action: PayloadAction<CartState>) => {
      state.cart = action.payload.cart;
    },
    addToCart: (
      state,
      action: PayloadAction<Cart & { cartItem: CartItemInterface }>,
    ) => {
      const cartItems = [...(state.cart?.cartItems ?? [])];

      console.log({ payload: action.payload });
      console.log({ cartItems });

      const itemIndex = cartItems.findIndex(
        (item) => item.id === action.payload.cartItem.id,
      );
      if (itemIndex >= 0) {
        cartItems.splice(itemIndex, 1, action.payload.cartItem);
      } else {
        cartItems.push(action.payload.cartItem);
      }

      state.cart.id = action.payload.id;
      state.cart.customer_id = action.payload.customer_id;
      state.cart.cartItems = cartItems;
    },
  },
});

export const { addCart, addToCart } = cartSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectCount = (state: RootState) => state.cart;

export default cartSlice.reducer;
