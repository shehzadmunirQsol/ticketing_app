import { NextPageWithLayout } from '~/pages/_app';
import Invoice from '~/components/OrderInvoice/Invoice';
import { useRouter } from 'next/router';

const InvoiceDetails: NextPageWithLayout = () => {
     return <>
    <Invoice admin={false} /></>
};

export default InvoiceDetails;