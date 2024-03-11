import { ethers } from 'ethers';
import { createSmartAccountClient } from '@biconomy/account';

type createSmartAccountType = {
  private_address: string;
};
export const createSmartAccount = async (props: createSmartAccountType) => {
  const provider = new ethers.providers.JsonRpcProvider(
    process.env.RPC_URL, // RPCURL
  );
  const signer = new ethers.Wallet(
    props?.private_address ??
      '1a9060d87234f853cad12f07c98c903ff8138f467a795f9faf0b7cfd48f52510', // Private key
    provider,
  );
  const biconomySmartAccountConfig: any = {
    signer: signer,
    bundlerUrl: process.env.BUNDLER_URL, // <-- Read about this at https://docs.biconomy.io/dashboard#bundler-url
    biconomyPaymasterApiKey: 'XiAgATBnh.d7c89c8b-e593-49ff-bc73-1654be157449', // <-- Read about at https://docs.biconomy.io/dashboard/paymaster
  };

  const smartAccount = await createSmartAccountClient(
    biconomySmartAccountConfig,
  );

  const smartAccountAddress = await smartAccount.getAccountAddress();

  console.log('Smart Account Address', smartAccountAddress);
  return smartAccount;
};
