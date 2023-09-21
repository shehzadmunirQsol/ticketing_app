import { NextPageWithLayout } from '~/pages/_app';
// import { prisma } from '~/server/prisma';
import ProductDetail from '~/components/app/productDescription/index';
// import { serverRouter } from '~/server/routers/_app';

const ProductDetails: NextPageWithLayout = (props) => {
  return <ProductDetail />;
};

export default ProductDetails;

// // This function gets called at build time
// export async function getStaticPaths() {
//   const today = new Date();

//   const events = await prisma.event.findMany({
//     orderBy: { created_at: 'desc' },
//     take: 20,
//     where: { draw_date: null, end_date: { gte: today } },
//     select: { id: true },
//   });

//   const paths = events.map((event) => ({
//     params: { id: event.id.toString() },
//   }));
//   return {
//     // Only `/posts/1` and `/posts/2` are generated at build time
//     paths: paths,
//     // Enable statically generating additional pages
//     // For example: `/posts/3`
//     fallback: true,
//   };
// }

// // This also gets called at build time
// export async function getStaticProps(ctx: any) {
//   // params contains the post `id`.
//   // If the route is like /posts/1, then params.id is 1

//   const trpc = serverRouter(ctx);
//   const payload = {
//     type: 'client' as 'admin' | 'client' | undefined,
//     lang_id: 1,
//     id: Number(ctx.params.id),
//   };

//   const response = await trpc.event.getEventsById(payload);
//   const data = JSON.parse(JSON.stringify(response.data));

//   // Pass post data to the page via props
//   return {
//     props: {
//       event: data,
//     },
//     // Re-generate the post at most once per second
//     // if a request comes in
//     revalidate: 60,
//   };
// }
