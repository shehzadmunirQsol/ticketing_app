import { createSlice } from '@reduxjs/toolkit';
import type { PayloadAction } from '@reduxjs/toolkit';
import { RootState } from '~/store/store';

interface Cart {
  id: number | null;
  customer_id: number | null;
}

export interface CartItemInterface {
  id: number;
  Event: {
    thumb: string;
    price: number;
    EventDescription: {
      name: string;
    }[];
  };
  subscription_type: 'weekly' | 'monthly' | 'quarterly' | null;
  cart_id: number;
  event_id: number;
  quantity: number;
  is_subscribe: boolean;
}
// Define a type for the slice state
interface CartState {
  cart: Cart & { cartItems: CartItemInterface[] };
  count: number;
  totalAmount: number;
}

// Define the initial state using that type
const initialState: CartState = {
  cart: {
    id: null,
    customer_id: null,
    cartItems: [],
  },
  count: 0,
  totalAmount: 0,
};

export const cartSlice = createSlice({
  name: 'cart',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    addCart: (state, action: PayloadAction<CartState>) => {
      state.cart = action.payload.cart;
      state.count = getTotalCount(state.cart?.cartItems ?? []);
      state.totalAmount = getTotalAmount(state.cart?.cartItems ?? []);
    },
    addToCart: (
      state,
      action: PayloadAction<Cart & { cartItem: CartItemInterface }>,
    ) => {
      const cartItems = [...(state.cart?.cartItems ?? [])];

      const itemIndex = cartItems.findIndex(
        (item) => item.id === action.payload.cartItem.id,
      );
      if (itemIndex >= 0) {
        cartItems.splice(itemIndex, 1, action.payload.cartItem);
      } else {
        cartItems.push(action.payload.cartItem);
      }

      state.count = getTotalCount(cartItems);
      state.totalAmount = getTotalAmount(cartItems);
      state.cart.id = action.payload.id;
      state.cart.customer_id = action.payload.customer_id;
      state.cart.cartItems = cartItems;
    },
    removeFromCart: (
      state,
      action: PayloadAction<{ cart_item_id: number }>,
    ) => {
      const cartItems = [...(state.cart?.cartItems ?? [])].filter(
        (item) => item.id !== action.payload.cart_item_id,
      );

      state.count = getTotalCount(cartItems);
      state.totalAmount = getTotalAmount(cartItems);
      state.cart.cartItems = cartItems;
    },
  },
});

function getTotalCount(cartItems: CartItemInterface[]): number {
  const totalCount = cartItems.reduce(
    (accumulator, current) => accumulator + current.quantity,
    0,
  );

  return totalCount;
}
function getTotalAmount(cartItems: CartItemInterface[]): number {
  console.log('cartItems', cartItems);
  const totalAmount = cartItems.reduce(
    (accumulator, current) =>
      accumulator + current.quantity * current.Event.price,
    0,
  );

  console.log('totalAmount', totalAmount);

  return totalAmount;
}

export const { addCart, addToCart, removeFromCart } = cartSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectCount = (state: RootState) => state.cart;

export default cartSlice.reducer;
