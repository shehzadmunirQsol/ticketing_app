import { useEffect, useState } from 'react';
import jqeury from 'jquery';

import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from '~/components/ui/dialog';
import { useToast } from '~/components/ui/use-toast';
import { trpc } from '~/utils/trpc';
import { LoadingDialog } from './loadingModal';
import { useForm } from 'react-hook-form';

import { Input } from '~/components/ui/input';

import { createFormPaymentSchema, createPaymentSchema } from '~/schema/payment';
import { zodResolver } from '@hookform/resolvers/zod';
import { useDispatch, useSelector } from 'react-redux';
import { RootState } from '~/store/store';
import { z } from 'zod';
import { userAuth } from '~/store/reducers/auth';
import { addCart } from '~/store/reducers/cart';
import { useRouter } from 'next/router';
import Script from 'next/script';
interface SettingDialogInterface {
  selectedItem: any;
  isModal: boolean;
  title: string;
  setTitle: any;
  setSelectedItem: any;
  setIsModal: any;
  type: string;
  setType: any;
}
export function CheckoutDialog(props: SettingDialogInterface) {
  const { user } = useSelector((state: RootState) => state.auth);
  const { cart, totalAmount } = useSelector((state: RootState) => state.cart);
  console.log({ user }, 'props?.selectedItem');

  console.log(user?.total_customer_id);
  const dispatch = useDispatch();
  const router = useRouter();

  const { toast } = useToast();
  const [loading, setLoading] = useState<boolean>(false);
  const [modelState, setModelState] = useState<boolean>(false);

  const wpwlOptions = {
    registrations: { requireCvv: true, hideInitialPaymentForms: true },
  };
  useEffect(() => {
    function addCustomElement() {
      // Create the HTML elements
      console.log(
        jqeury('form.wpwl-form-card').find('.wpwl-button'),
        'data should be loaded',
      );
      const createRegistrationHtml =
        '<div class="customLabel">Store payment details?</div><div class="customInput"><input type="checkbox" name="createRegistration" value="true" /></div>';
      jqeury('form.wpwl-form-card')
        .find('.wpwl-button')
        .before(createRegistrationHtml);
    }

    // Delay the execution of addCustomElement by 1000 milliseconds (1 second)
    setTimeout(addCustomElement, 20000);
  }, []);
  return (
    <>
      <Dialog open={props?.isModal} onOpenChange={(e) => props.setIsModal(e)}>
        <DialogContent className="sm:max-w-[450px]  max-h-[calc(100%-70px)] overflow-y-scroll scroll-hide">
          <Script
            src={`https://eu-test.oppwa.com/v1/paymentWidgets.js?checkoutId=${props?.selectedItem?.checkoutID}`}
            onReady={() => {
              console.log('Script has loaded');
              const wpwlOptions = {
                registrations: {
                  requireCvv: true,
                  hideInitialPaymentForms: true,
                },
              };
              const createRegistrationHtml =
                '<div class="customLabel">Store payment details?</div><div class="customInput"><input type="checkbox" name="createRegistration" value="true" /></div>';
              jqeury('form.wpwl-form-card')
                .find('.wpwl-button')
                .before(createRegistrationHtml);
            }}
          ></Script>
          <form
            action={`${process.env.NEXT_PUBLIC_BASE_URL}/checkout`}
            className="paymentWidgets justify-start   lg:justify-center md:justify-center items-center px-2 lg:px-6 py-2 space-y-2 text-black"
            data-brands="VISA MASTER AMEX "
          ></form>
        </DialogContent>
      </Dialog>
      <LoadingDialog open={loading} text={'Saving data...'} />
    </>
  );
}
