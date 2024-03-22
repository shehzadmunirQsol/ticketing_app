import { signJWT, verifyJWT } from '~/utils/jwt';
import { loginCustomerSchema } from '~/schema/auth';
import { prisma } from '~/server/prisma';
import { createSmartAccount } from '../web3-controller/createAccount';
// import createSmartAccount fro
/* 
 ---- input ----
 email
 password 
*/
export async function loginCustomer(req: any, res: any) {
  // const input = req.body;

  try {
    if (!req.body)
      return res.status(400).send({ message: 'payload not found' });

    const input = req.body;

    // const input = JSON.parse(req.body as any);
    const validate = loginCustomerSchema.safeParse(input);

    if (!validate.success)
      return res.status(400).send({
        message:
          validate?.error && validate?.error?.errors[0]?.message
            ? validate?.error?.errors[0]?.message
            : 'Bad Request',
      });
    let customer: any;
    customer = await prisma.user.findFirst({
      where: {
        email: validate.data?.email,
      },
      include: {
        Role: true,
      },
    });
    const { private_address, ...inputData } = validate.data;
    const decodePrivateAddress: any = await verifyJWT(private_address);
    const smartAccount = await createSmartAccount({
      private_address: decodePrivateAddress.address,
    });
    const smartAccountAddress = await smartAccount.getAccountAddress();
    if (!customer) {
      customer = await prisma.user.create({
        data: {
          ...inputData,
          wallet_address: smartAccountAddress,
        },
        include: {
          Role: true,
        },
      });

      return res.status(201).send({ customer, is_registerd: false });
    }

    //  console.log('address : ', smartAccountAddress);
    if (!customer?.wallet_address && smartAccountAddress) {
      customer = await prisma.user.update({
        where: {
          id: customer.id,
        },
        data: {
          wallet_address: smartAccountAddress,
        },
        include: {
          Role: true,
        },
      });
    }
    if (!customer?.is_registerd && !customer?.role_id) {
      return res
        .status(201)
        .send({ customer, is_registerd: customer?.is_registerd });
    }
    if (!customer?.device_id || customer?.device_id !== inputData?.device_id) {
      customer = await prisma.user.update({
        where: {
          id: customer.id,
        },
        data: {
          device_id: inputData?.device_id,
        },
        include: {
          Role: true,
        },
      });
    }
    const jwt = signJWT({
      email: customer.email,
      role_id: customer.role_id,
      first_name: customer.first_name,
      role: customer?.Role?.name,

      id: customer.id,
    });
    const { password, ...data } = customer;
    return res.status(200).send({ customer: data, jwt });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string });
  }
}
