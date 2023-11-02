import { NextPageWithLayout } from '~/pages/_app';
import Invoice from '~/components/OrderInvoice/Invoice';

const InvoiceDetails: NextPageWithLayout = () => <Invoice admin={false} />;

export default InvoiceDetails;
