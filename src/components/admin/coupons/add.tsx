import CouponForm from '~/components/common/forms/coupon';
import GlobalBack from '~/components/common/globalBack';

export default function AddCoupon() {
  return (
    <div className="p-8 space-y-8">
      <div className="flex items-center justify-between">
        <h2 className="text-4xl font-medium">Add Coupon</h2>
        <GlobalBack/>
      </div>
      <CouponForm />
    </div>
  );
}
