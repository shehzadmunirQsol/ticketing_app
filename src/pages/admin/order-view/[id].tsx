import { NextPageWithLayout } from '~/pages/_app';
import Invoice from '~/components/OrderInvoice/Invoice';

const InvoiceDetails: NextPageWithLayout = () => <Invoice admin={true} />;

export default InvoiceDetails;