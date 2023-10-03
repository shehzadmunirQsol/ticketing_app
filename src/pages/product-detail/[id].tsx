import { NextPageWithLayout } from '~/pages/_app';
import ProductDetail from '~/components/app/productDescription/index';

const ProductDetails: NextPageWithLayout = () => <ProductDetail />;

export default ProductDetails;

// This also gets called at build time
// export const getServerSideProps: GetServerSideProps = async (ctx) => {
//   const { id: eventId } = URIDecoder(ctx?.params?.id);

//   const trpc = serverRouter(ctx);
//   const payload = {
//     type: 'client' as 'admin' | 'client' | undefined,
//     lang_id: 1,
//     id: Number(eventId),
//   };

//   const response = await trpc.event.getEventsById(payload);
//   const data = JSON.parse(JSON.stringify(response.data));

//   // Pass post data to the page via props
//   return {
//     props: {
//       eventData: { event: data, ticketPurchased: response.ticketPurchased },
//     },
//   };
// };
