export async function totalProcessingPayment(req: any, res: any) {
  const input = { ...req.query };
  delete input.routes;

  try {
    return res.status(200).send({ data: 'data', success: true });
  } catch (err: any) {
    res.status(500).send({ message: err.message as string, success: false });
  }
}
