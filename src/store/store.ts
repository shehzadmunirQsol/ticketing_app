import { configureStore } from '@reduxjs/toolkit';
import layoutReducer from './reducers/layout';
import adminLayoutReducer from './reducers/admin_layout';

const store = configureStore({
  reducer: {
    layout: layoutReducer,
    adminLayout: adminLayoutReducer,
  },
});

// Infer the `RootState` and `AppDispatch` types from the store itself
export type RootState = ReturnType<typeof store.getState>;
// Inferred type: {posts: PostsState, comments: CommentsState, users: UsersState}
export type AppDispatch = typeof store.dispatch;

export default store;
