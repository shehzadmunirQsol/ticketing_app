import { NextPageWithLayout } from '~/pages/_app';

const IndexPage: NextPageWithLayout = () => (
  <>
    <div className="relative   flex min-h-screen max-w-4xl mx-auto bg-white items-center  justify-center ">
      <div
        className="mx-auto h-full w-full  rounded-lg bg-white px-2 py-4 "
        id="divToPrint"
      >
        <div className="mb-3 flex items-center justify-between">
          <div className="flex items-center"></div>
          <div className="text-gray-700">
            <div className="mb-2 text-xl font-bold">INVOICE</div>
            <div className="text-sm">Date: 12/02/2024</div>
            <div className="text-sm uppercase">
              Invoice #: INV
              {/* {customInvoiceNumberHandler(orderData?.data?.id, 4)} */}
              11
            </div>
            <div className="text-sm">Payment Type: card</div>
          </div>
        </div>
        <div className="mb-8 border-b-2 border-gray-300 pb-4 text-sm">
          <h2 className="mb-2 text-2xl font-bold">Bill To:</h2>
          <div className="mb-1 capitalize text-gray-700">
            {/* {orderData?.data?.store_customers?.full_name} */}shehzad
          </div>
          {/* {orderData?.data?.is_card ? (
            <>
              <div className="mb-1 line-clamp-2 text-gray-700">
                123 addres
              </div>
            </>
          ) : (
            ''
          )} */}

          <div className="text-gray-700">
            {/* {orderData?.data?.store_customers?.email}1 */}1
          </div>
        </div>
        <table className="mb-8 w-full text-left">
          <thead>
            <tr>
              <th className="py-2 font-bold uppercase text-gray-700">
                Description
              </th>
              <th className="py-2 font-bold uppercase text-gray-700">Qty.</th>
              <th className="py-2 font-bold uppercase text-gray-700">Type</th>
              <th className="py-2 text-end font-bold uppercase text-gray-700">
                Price
              </th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td className="py-4 text-gray-700">
                {/* {orderData?.data?.store_nfts?.name} */}
              </td>
              <td className="py-4 text-gray-700">1</td>
              <td className="py-4 text-gray-700">
                {/* {orderData?.data?.is_card ? 'Card' : 'Wallet'} */}Wallet
              </td>
              <td className="py-4 text-end text-gray-700"></td>
            </tr>
          </tbody>
        </table>

        <div className="mb-2 flex  justify-end text-right">
          <div className="mr-2 text-gray-700">Service Fee:</div>
          <div className="text-gray-700"></div>
        </div>
        <div className="mb-2 flex justify-end">
          <div className="mr-2 text-gray-700">Total:</div>
          <div className="text-xl font-bold text-gray-700"></div>
        </div>
        {/* <div className="mb-8 border-t-2 border-gray-300 pt-8">
                <div className="mb-2 text-gray-700">
                  Please make checks payable to Your Company Name and mail to:
                </div>
                <div className="text-gray-700">
                  123 Main St., Anytown, USA 12345
                </div>
              </div> */}
      </div>
    </div>
  </>
);

export default IndexPage;
