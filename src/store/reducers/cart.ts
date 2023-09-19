import { createSlice } from '@reduxjs/toolkit';
import type { PayloadAction } from '@reduxjs/toolkit';
import { RootState } from '~/store/store';

interface Cart {
  id: number | null;
  customer_id: number | null;
  isDiscount: boolean;
  discount: number;
  isPercentage: boolean;
}

export interface CartItemInterface {
  id: number;
  Event: {
    thumb: string;
    price: number;
    end_date: Date | null;
    tickets_sold: number | null;
    user_ticket_limit: number;
    total_tickets: number;

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
    isDiscount: false,
    discount: 0,
    isPercentage: false,
    cartItems: [],
  },
  count: 0,
  totalAmount: 0,
};

type AddToCartType = Pick<Cart, 'id' | 'customer_id'>;

export const cartSlice = createSlice({
  name: 'cart',
  // `createSlice` will infer the state type from the `initialState` argument
  initialState,
  reducers: {
    addCart: (
      state,
      action: PayloadAction<Cart & { cartItems: CartItemInterface[] }>,
    ) => {
      state.cart = action.payload;
      state.count = getTotalCount(action.payload?.cartItems ?? []);
      state.totalAmount = getTotalAmount(action.payload?.cartItems ?? []);
    },
    addToCart: (
      state,
      action: PayloadAction<AddToCartType & { cartItem: CartItemInterface }>,
    ) => {
      const cartItems = [...(state.cart?.cartItems ?? [])];

      const itemIndex = cartItems.findIndex(
        (item) => item.event_id === action.payload.cartItem.event_id,
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
    removeFromCart: (state, action: PayloadAction<{ event_id: number }>) => {
      const cartItems = [...(state.cart?.cartItems ?? [])].filter(
        (item) => item.event_id !== action.payload.event_id,
      );

      state.count = getTotalCount(cartItems);
      state.totalAmount = getTotalAmount(cartItems);
      state.cart.cartItems = cartItems;
    },
    addDiscount: (
      state,
      action: PayloadAction<{
        isDiscount: boolean;
        discount: number;
        isPercentage: boolean;
      }>,
    ) => {
      state.cart.isDiscount = action.payload.isDiscount;
      state.cart.discount = action.payload.discount;
      state.cart.isPercentage = action.payload.isPercentage;
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
  const totalAmount = cartItems.reduce(
    (accumulator, current) =>
      accumulator + current.quantity * current.Event.price,
    0,
  );

  return totalAmount;
}

export const { addCart, addToCart, removeFromCart, addDiscount } =
  cartSlice.actions;

// Other code such as selectors can use the imported `RootState` type
export const selectCount = (state: RootState) => state.cart;

export default cartSlice.reducer;
